//! Handler specialization — meta-time reduction of statically-known handlers.
//!
//! Runs at the tail of `meta_processor::process`, after monomorphization and
//! before compaction. Goal: rewrite `run { body } handle Name` install sites
//! whose handler is statically known into direct, inlined op calls, so the
//! runtime AST eventually contains no handler closures (chunks 14–16).
//!
//! Chunk 6 implements the simplest case: a `WithFn` whose handler body has no
//! free captures and whose direct call sites all live in the same enclosing
//! block. Each such WithFn is lifted to a top-level `__op_<op>_<id>` FnDecl;
//! direct calls to `op_name` in sibling stmts are rewritten to call that fn.
//! The `WithFn` install is left in place so transitive handler routing
//! (handler_transform.rs) continues to work for callees this chunk doesn't
//! cover (lifted to chunks 8+).

use std::collections::{HashMap, HashSet};

use super::conversion::collect_free_vars;
use super::runtime_ast::{RuntimeAst, RuntimeExpr, RuntimeStmt};
use crate::util::node_id::RuntimeNodeId;

pub fn specialize(ast: &mut RuntimeAst) {
    // Iterate to a fixed point. Each pass:
    //   * recomputes `globals` so newly-minted lifted fns / clones count as
    //     fn refs (and don't get mistaken for captures);
    //   * walks only the roots that appeared since the previous pass —
    //     existing ones have already been visited.
    let mut visited_through = 0;
    loop {
        let globals = global_names(ast);
        let total = ast.sem_root_stmts.len();
        if visited_through >= total { break; }
        let new_roots: Vec<RuntimeNodeId> =
            ast.sem_root_stmts[visited_through..total].to_vec();
        visited_through = total;
        for id in new_roots {
            visit_stmt(ast, id, &globals);
        }
    }
}

fn global_names(ast: &RuntimeAst) -> HashSet<String> {
    let mut g: HashSet<String> = HashSet::new();
    for stmt in ast.stmts.values() {
        match stmt {
            RuntimeStmt::FnDecl { name, .. } => { g.insert(name.clone()); }
            RuntimeStmt::EffectDecl { ops, .. } => {
                for op in ops { g.insert(op.name.clone()); }
            }
            _ => {}
        }
    }
    for builtin in ["readfile", "writefile", "to_string", "to_int", "free", "print"] {
        g.insert(builtin.to_string());
    }
    g
}

enum StmtPlan {
    Block(Vec<RuntimeNodeId>),
    WithBody(RuntimeNodeId),
    If { cond: RuntimeNodeId, body: RuntimeNodeId, else_branch: Option<RuntimeNodeId> },
    While { cond: RuntimeNodeId, body: RuntimeNodeId },
    Body(RuntimeNodeId),
    Expr(RuntimeNodeId),
    Nothing,
}

fn visit_stmt(ast: &mut RuntimeAst, stmt_id: RuntimeNodeId, globals: &HashSet<String>) {
    let plan = match ast.get_stmt(stmt_id) {
        Some(RuntimeStmt::WithFn { body, .. }) => StmtPlan::WithBody(*body),
        Some(RuntimeStmt::WithCtl { body, .. }) => StmtPlan::WithBody(*body),
        Some(RuntimeStmt::Block(stmts)) => StmtPlan::Block(stmts.clone()),
        Some(RuntimeStmt::If { cond, body, else_branch }) => StmtPlan::If {
            cond: *cond, body: *body, else_branch: *else_branch,
        },
        Some(RuntimeStmt::WhileLoop { cond, body }) => StmtPlan::While { cond: *cond, body: *body },
        Some(RuntimeStmt::ForEach { body, .. }) => StmtPlan::Body(*body),
        Some(RuntimeStmt::FnDecl { body, .. }) => StmtPlan::Body(*body),
        Some(RuntimeStmt::VarDecl { expr, .. })
        | Some(RuntimeStmt::ExprStmt(expr))
        | Some(RuntimeStmt::Return(Some(expr)))
        | Some(RuntimeStmt::Print(expr))
        | Some(RuntimeStmt::Assign { expr, .. }) => StmtPlan::Expr(*expr),
        _ => StmtPlan::Nothing,
    };
    match plan {
        StmtPlan::Block(stmts) => {
            let removed = specialize_block(ast, &stmts, globals);
            let remaining: Vec<RuntimeNodeId> = stmts.iter().copied()
                .filter(|id| !removed.contains(id))
                .collect();
            if remaining.len() != stmts.len() {
                if let Some(RuntimeStmt::Block(s)) = ast.stmts.get_mut(&stmt_id) {
                    *s = remaining.clone();
                }
            }
            for c in remaining { visit_stmt(ast, c, globals); }
        }
        StmtPlan::WithBody(b) | StmtPlan::Body(b) => visit_stmt(ast, b, globals),
        StmtPlan::If { cond, body, else_branch } => {
            visit_expr(ast, cond, globals);
            visit_stmt(ast, body, globals);
            if let Some(e) = else_branch { visit_stmt(ast, e, globals); }
        }
        StmtPlan::While { cond, body } => {
            visit_expr(ast, cond, globals);
            visit_stmt(ast, body, globals);
        }
        StmtPlan::Expr(e) => visit_expr(ast, e, globals),
        StmtPlan::Nothing => {}
    }
}

fn visit_expr(ast: &mut RuntimeAst, expr_id: RuntimeNodeId, globals: &HashSet<String>) {
    let children: Vec<RuntimeNodeId> = match ast.get_expr(expr_id) {
        Some(RuntimeExpr::Call { args, .. }) => args.clone(),
        Some(RuntimeExpr::Add(a, b) | RuntimeExpr::Sub(a, b) | RuntimeExpr::Mult(a, b)
            | RuntimeExpr::Div(a, b) | RuntimeExpr::Equals(a, b) | RuntimeExpr::NotEquals(a, b)
            | RuntimeExpr::Lt(a, b) | RuntimeExpr::Gt(a, b) | RuntimeExpr::Lte(a, b)
            | RuntimeExpr::Gte(a, b) | RuntimeExpr::And(a, b) | RuntimeExpr::Or(a, b)) => vec![*a, *b],
        Some(RuntimeExpr::Not(a)) => vec![*a],
        Some(RuntimeExpr::List(elems) | RuntimeExpr::Tuple(elems)) => elems.clone(),
        Some(RuntimeExpr::Lambda { body, .. }) => {
            visit_stmt(ast, *body, globals);
            return;
        }
        Some(RuntimeExpr::DotCall { object, args, .. }) => {
            let mut v = vec![*object];
            v.extend(args.iter().copied());
            v
        }
        Some(RuntimeExpr::ClassLiteral { fields, .. }) => fields.iter().map(|(_, id)| *id).collect(),
        _ => return,
    };
    for c in children { visit_expr(ast, c, globals); }
}

// ── Block-level reduction ────────────────────────────────────────────────────

fn specialize_block(ast: &mut RuntimeAst, stmts: &[RuntimeNodeId], globals: &HashSet<String>) -> HashSet<RuntimeNodeId> {
    let mut removed: HashSet<RuntimeNodeId> = HashSet::new();
    for (i, &stmt_id) in stmts.iter().enumerate() {
        // ── Chunks 6 + 10: WithFn { op }, capture-aware lift ──
        if let Some(RuntimeStmt::WithFn { op_name, params, body, .. }) = ast.get_stmt(stmt_id) {
            let op_name = op_name.clone();
            let op_params: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
            let body = *body;

            let free = collect_free_vars(ast, &[body]);
            let captures: Vec<String> = free.into_iter()
                .filter(|n| !op_params.contains(n) && !globals.contains(n))
                .collect();

            let mut full_params = captures.clone();
            full_params.extend(op_params.iter().cloned());

            let new_fn = format!("__op_{}_{}", op_name, stmt_id.0);
            let new_decl = RuntimeNodeId(ast.next_id);
            ast.next_id += 1;
            ast.stmts.insert(new_decl, RuntimeStmt::FnDecl {
                name: new_fn.clone(),
                params: full_params,
                type_params: vec![],
                body,
            });
            ast.sem_root_stmts.push(new_decl);

            for &sib in &stmts[i + 1..] {
                rewrite_call_stmt_with_prepend(ast, sib, &op_name, &new_fn, &captures);
            }
            specialize_transitive(ast, stmts, i + 1, &op_name, &new_fn, &captures);
            // Chunk 14: the WithFn install is now dead — every direct + transitive
            // user of `op_name` in this block has been rewritten to the lifted fn.
            removed.insert(stmt_id);
            continue;
        }

        // ── Chunk 7: WithCtl { op } where body is a single tail Resume ──
        if let Some(RuntimeStmt::WithCtl { op_name, params, body, .. }) = ast.get_stmt(stmt_id) {
            let op_name = op_name.clone();
            let params: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
            let body = *body;

            let Some(resume_expr) = single_tail_resume(ast, body) else { continue };

            let free = collect_free_vars(ast, &[body]);
            let captures_extra = free.iter().any(|n| !params.contains(n) && !globals.contains(n));
            if captures_extra { continue; }

            // Build `fn __op_<op>_<id>(params) { return <resume_expr>; }`. We do
            // NOT mutate the original Resume — it is still reachable through
            // WithCtl. Future cps_transform may inspect it, so leave intact.
            let ret_stmt = RuntimeNodeId(ast.next_id);
            ast.next_id += 1;
            ast.stmts.insert(ret_stmt, RuntimeStmt::Return(resume_expr));

            let new_body = RuntimeNodeId(ast.next_id);
            ast.next_id += 1;
            ast.stmts.insert(new_body, RuntimeStmt::Block(vec![ret_stmt]));

            let new_fn = format!("__op_{}_{}", op_name, stmt_id.0);
            let new_decl = RuntimeNodeId(ast.next_id);
            ast.next_id += 1;
            ast.stmts.insert(new_decl, RuntimeStmt::FnDecl {
                name: new_fn.clone(),
                params,
                type_params: vec![],
                body: new_body,
            });
            ast.sem_root_stmts.push(new_decl);

            for &sib in &stmts[i + 1..] {
                rewrite_call_stmt(ast, sib, &op_name, &new_fn);
            }
            specialize_transitive(ast, stmts, i + 1, &op_name, &new_fn, &[]);
            // Chunk 15: the WithCtl install is dead — call sites all go to the
            // lifted fn now. Non-tail-resumptive WithCtl cases hit `continue`
            // above and are left in place for cps_transform.
            removed.insert(stmt_id);
        }
    }
    removed
}

// ── Chunk 8: specialize transitive callees ───────────────────────────────────
//
// After a WithFn/WithCtl has been lifted to `new_op_fn`, any sibling stmt may
// call a user fn `f` that itself directly invokes `op_name`. Such an `f` is
// cloned to `f__<op>_<id>`, the clone's op call sites are rewritten to point at
// `new_op_fn`, and sibling calls to `f` are redirected to the specialized copy.

fn specialize_transitive(
    ast: &mut RuntimeAst,
    stmts: &[RuntimeNodeId],
    after: usize,
    op_name: &str,
    new_op_fn: &str,
    captures: &[String],
) {
    let mut callees: HashSet<String> = HashSet::new();
    for &sib in &stmts[after..] {
        collect_direct_callees_stmt(ast, sib, &mut callees);
    }
    callees.remove(new_op_fn);

    let fn_index = index_fndecls(ast);
    let mut rewrites: Vec<(String, String)> = Vec::new();
    for callee in callees {
        let Some(&(decl_id, body)) = fn_index.get(&callee) else { continue };
        let orig_params = match ast.get_stmt(decl_id) {
            Some(RuntimeStmt::FnDecl { params, .. }) => params.clone(),
            _ => continue,
        };
        if !uses_op_directly(ast, body, op_name) { continue; }

        // Clone body and rewrite op-calls (prepending captures as args, since
        // those captures will be the clone's own params).
        let new_body = deep_clone_subtree(ast, body);
        rewrite_call_stmt_with_prepend(ast, new_body, op_name, new_op_fn, captures);

        let specialized = format!("{}__via_{}", callee, new_op_fn);
        if calls_self(ast, body, &callee) {
            // Self-recursive calls in the clone need both the new name AND the
            // captures threaded through (so deeper levels still see them).
            rewrite_call_stmt_with_prepend(ast, new_body, &callee, &specialized, captures);
        }

        let mut full_params = captures.to_vec();
        full_params.extend(orig_params.iter().cloned());

        let new_decl = RuntimeNodeId(ast.next_id);
        ast.next_id += 1;
        ast.stmts.insert(new_decl, RuntimeStmt::FnDecl {
            name: specialized.clone(),
            params: full_params,
            type_params: vec![],
            body: new_body,
        });
        ast.sem_root_stmts.push(new_decl);
        rewrites.push((callee, specialized));
    }

    for (old, new) in rewrites {
        for &sib in &stmts[after..] {
            rewrite_call_stmt_with_prepend(ast, sib, &old, &new, captures);
        }
    }
}

fn index_fndecls(ast: &RuntimeAst) -> HashMap<String, (RuntimeNodeId, RuntimeNodeId)> {
    let mut out = HashMap::new();
    for (&id, stmt) in &ast.stmts {
        if let RuntimeStmt::FnDecl { name, body, .. } = stmt {
            out.entry(name.clone()).or_insert((id, *body));
        }
    }
    out
}

fn uses_op_directly(ast: &RuntimeAst, body: RuntimeNodeId, op_name: &str) -> bool {
    let mut found = false;
    scan_op_stmt(ast, body, op_name, &mut found);
    found
}

fn scan_op_stmt(ast: &RuntimeAst, stmt_id: RuntimeNodeId, op_name: &str, found: &mut bool) {
    if *found { return; }
    match ast.get_stmt(stmt_id) {
        Some(RuntimeStmt::Block(stmts)) => {
            for &c in &stmts.clone() { scan_op_stmt(ast, c, op_name, found); }
        }
        Some(RuntimeStmt::If { cond, body, else_branch }) => {
            scan_op_expr(ast, *cond, op_name, found);
            scan_op_stmt(ast, *body, op_name, found);
            if let Some(e) = else_branch { scan_op_stmt(ast, *e, op_name, found); }
        }
        Some(RuntimeStmt::WhileLoop { cond, body }) => {
            scan_op_expr(ast, *cond, op_name, found);
            scan_op_stmt(ast, *body, op_name, found);
        }
        Some(RuntimeStmt::ForEach { body, .. }) => scan_op_stmt(ast, *body, op_name, found),
        Some(RuntimeStmt::VarDecl { expr, .. })
        | Some(RuntimeStmt::ExprStmt(expr))
        | Some(RuntimeStmt::Return(Some(expr)))
        | Some(RuntimeStmt::Print(expr))
        | Some(RuntimeStmt::Assign { expr, .. }) => scan_op_expr(ast, *expr, op_name, found),
        _ => {}
    }
}

fn scan_op_expr(ast: &RuntimeAst, expr_id: RuntimeNodeId, op_name: &str, found: &mut bool) {
    if *found { return; }
    let children: Vec<RuntimeNodeId> = match ast.get_expr(expr_id) {
        Some(RuntimeExpr::Call { callee, args }) => {
            if callee == op_name { *found = true; return; }
            args.clone()
        }
        Some(RuntimeExpr::Add(a, b) | RuntimeExpr::Sub(a, b) | RuntimeExpr::Mult(a, b)
            | RuntimeExpr::Div(a, b) | RuntimeExpr::Equals(a, b) | RuntimeExpr::NotEquals(a, b)
            | RuntimeExpr::Lt(a, b) | RuntimeExpr::Gt(a, b) | RuntimeExpr::Lte(a, b)
            | RuntimeExpr::Gte(a, b) | RuntimeExpr::And(a, b) | RuntimeExpr::Or(a, b)) => vec![*a, *b],
        Some(RuntimeExpr::Not(a)) => vec![*a],
        Some(RuntimeExpr::List(elems) | RuntimeExpr::Tuple(elems)) => elems.clone(),
        Some(RuntimeExpr::Lambda { .. }) => return,
        Some(RuntimeExpr::DotCall { object, args, .. }) => {
            let mut v = vec![*object];
            v.extend(args.iter().copied());
            v
        }
        Some(RuntimeExpr::ClassLiteral { fields, .. }) => fields.iter().map(|(_, id)| *id).collect(),
        _ => return,
    };
    for c in children { scan_op_expr(ast, c, op_name, found); }
}

fn calls_self(ast: &RuntimeAst, body: RuntimeNodeId, name: &str) -> bool {
    let mut found = false;
    scan_op_stmt(ast, body, name, &mut found);
    found
}

fn collect_direct_callees_stmt(ast: &RuntimeAst, stmt_id: RuntimeNodeId, out: &mut HashSet<String>) {
    match ast.get_stmt(stmt_id) {
        Some(RuntimeStmt::Block(stmts)) => for &c in &stmts.clone() { collect_direct_callees_stmt(ast, c, out); }
        Some(RuntimeStmt::If { cond, body, else_branch }) => {
            collect_direct_callees_expr(ast, *cond, out);
            collect_direct_callees_stmt(ast, *body, out);
            if let Some(e) = else_branch { collect_direct_callees_stmt(ast, *e, out); }
        }
        Some(RuntimeStmt::WhileLoop { cond, body }) => {
            collect_direct_callees_expr(ast, *cond, out);
            collect_direct_callees_stmt(ast, *body, out);
        }
        Some(RuntimeStmt::ForEach { body, .. }) => collect_direct_callees_stmt(ast, *body, out),
        Some(RuntimeStmt::VarDecl { expr, .. })
        | Some(RuntimeStmt::ExprStmt(expr))
        | Some(RuntimeStmt::Return(Some(expr)))
        | Some(RuntimeStmt::Print(expr))
        | Some(RuntimeStmt::Assign { expr, .. }) => collect_direct_callees_expr(ast, *expr, out),
        _ => {}
    }
}

fn collect_direct_callees_expr(ast: &RuntimeAst, expr_id: RuntimeNodeId, out: &mut HashSet<String>) {
    let children: Vec<RuntimeNodeId> = match ast.get_expr(expr_id) {
        Some(RuntimeExpr::Call { callee, args }) => {
            out.insert(callee.clone());
            args.clone()
        }
        Some(RuntimeExpr::Add(a, b) | RuntimeExpr::Sub(a, b) | RuntimeExpr::Mult(a, b)
            | RuntimeExpr::Div(a, b) | RuntimeExpr::Equals(a, b) | RuntimeExpr::NotEquals(a, b)
            | RuntimeExpr::Lt(a, b) | RuntimeExpr::Gt(a, b) | RuntimeExpr::Lte(a, b)
            | RuntimeExpr::Gte(a, b) | RuntimeExpr::And(a, b) | RuntimeExpr::Or(a, b)) => vec![*a, *b],
        Some(RuntimeExpr::Not(a)) => vec![*a],
        Some(RuntimeExpr::List(elems) | RuntimeExpr::Tuple(elems)) => elems.clone(),
        Some(RuntimeExpr::Lambda { .. }) => return,
        Some(RuntimeExpr::DotCall { object, args, .. }) => {
            let mut v = vec![*object];
            v.extend(args.iter().copied());
            v
        }
        Some(RuntimeExpr::ClassLiteral { fields, .. }) => fields.iter().map(|(_, id)| *id).collect(),
        _ => return,
    };
    for c in children { collect_direct_callees_expr(ast, c, out); }
}

fn deep_clone_subtree(ast: &mut RuntimeAst, stmt_id: RuntimeNodeId) -> RuntimeNodeId {
    use super::monomorphize::clone_stmt;
    let mut new_stmts: HashMap<RuntimeNodeId, RuntimeStmt> = HashMap::new();
    let mut new_exprs: HashMap<RuntimeNodeId, RuntimeExpr> = HashMap::new();
    let mut stmt_map: HashMap<RuntimeNodeId, RuntimeNodeId> = HashMap::new();
    let mut expr_map: HashMap<RuntimeNodeId, RuntimeNodeId> = HashMap::new();
    let mut next_id = ast.next_id;
    let new_root = clone_stmt(ast, stmt_id, &mut next_id, &mut new_stmts, &mut new_exprs, &mut stmt_map, &mut expr_map);
    ast.next_id = next_id;
    ast.stmts.extend(new_stmts);
    ast.exprs.extend(new_exprs);
    new_root
}

/// Detect tail-resumptive ctl bodies: exactly one `Resume(opt_expr)` stmt,
/// either bare or wrapped in a single-stmt Block. Returns the resume value
/// (None for unit resume). Returns `None` for anything else — multishot,
/// abort, computation-before-resume, etc.
fn single_tail_resume(ast: &RuntimeAst, body: RuntimeNodeId) -> Option<Option<RuntimeNodeId>> {
    let stmt = ast.get_stmt(body)?;
    match stmt {
        RuntimeStmt::Resume(e) => Some(*e),
        RuntimeStmt::Block(stmts) if stmts.len() == 1 => {
            match ast.get_stmt(stmts[0])? {
                RuntimeStmt::Resume(e) => Some(*e),
                _ => None,
            }
        }
        _ => None,
    }
}

fn rewrite_call_stmt(ast: &mut RuntimeAst, stmt_id: RuntimeNodeId, op_name: &str, new_fn: &str) {
    rewrite_call_stmt_with_prepend(ast, stmt_id, op_name, new_fn, &[]);
}

fn rewrite_call_stmt_with_prepend(
    ast: &mut RuntimeAst,
    stmt_id: RuntimeNodeId,
    op_name: &str,
    new_fn: &str,
    prepend: &[String],
) {
    let plan = match ast.get_stmt(stmt_id) {
        Some(RuntimeStmt::Block(stmts)) => StmtPlan::Block(stmts.clone()),
        Some(RuntimeStmt::If { cond, body, else_branch }) => StmtPlan::If {
            cond: *cond, body: *body, else_branch: *else_branch,
        },
        Some(RuntimeStmt::WhileLoop { cond, body }) => StmtPlan::While { cond: *cond, body: *body },
        Some(RuntimeStmt::ForEach { body, .. }) => StmtPlan::Body(*body),
        Some(RuntimeStmt::VarDecl { expr, .. })
        | Some(RuntimeStmt::ExprStmt(expr))
        | Some(RuntimeStmt::Return(Some(expr)))
        | Some(RuntimeStmt::Print(expr))
        | Some(RuntimeStmt::Assign { expr, .. }) => StmtPlan::Expr(*expr),
        _ => StmtPlan::Nothing,
    };
    match plan {
        StmtPlan::Block(stmts) => for c in stmts { rewrite_call_stmt_with_prepend(ast, c, op_name, new_fn, prepend); }
        StmtPlan::If { cond, body, else_branch } => {
            rewrite_call_expr_with_prepend(ast, cond, op_name, new_fn, prepend);
            rewrite_call_stmt_with_prepend(ast, body, op_name, new_fn, prepend);
            if let Some(e) = else_branch { rewrite_call_stmt_with_prepend(ast, e, op_name, new_fn, prepend); }
        }
        StmtPlan::While { cond, body } => {
            rewrite_call_expr_with_prepend(ast, cond, op_name, new_fn, prepend);
            rewrite_call_stmt_with_prepend(ast, body, op_name, new_fn, prepend);
        }
        StmtPlan::Body(b) => rewrite_call_stmt_with_prepend(ast, b, op_name, new_fn, prepend),
        StmtPlan::Expr(e) => rewrite_call_expr_with_prepend(ast, e, op_name, new_fn, prepend),
        _ => {}
    }
}

fn rewrite_call_expr_with_prepend(
    ast: &mut RuntimeAst,
    expr_id: RuntimeNodeId,
    op_name: &str,
    new_fn: &str,
    prepend: &[String],
) {
    // First, if this is a matching Call, prepend new arg ids (allocated outside borrow).
    let mut prepended_args: Vec<RuntimeNodeId> = Vec::new();
    if !prepend.is_empty() {
        if let Some(RuntimeExpr::Call { callee, .. }) = ast.exprs.get(&expr_id) {
            if callee == op_name {
                for name in prepend {
                    let id = RuntimeNodeId(ast.next_id);
                    ast.next_id += 1;
                    ast.exprs.insert(id, RuntimeExpr::Variable(name.clone()));
                    prepended_args.push(id);
                }
            }
        }
    }
    let children: Vec<RuntimeNodeId> = match ast.exprs.get_mut(&expr_id) {
        Some(RuntimeExpr::Call { callee, args }) => {
            if callee == op_name {
                *callee = new_fn.to_string();
                let mut new_args = prepended_args;
                new_args.extend(args.iter().copied());
                *args = new_args;
            }
            args.clone()
        }
        Some(RuntimeExpr::Add(a, b) | RuntimeExpr::Sub(a, b) | RuntimeExpr::Mult(a, b)
            | RuntimeExpr::Div(a, b) | RuntimeExpr::Equals(a, b) | RuntimeExpr::NotEquals(a, b)
            | RuntimeExpr::Lt(a, b) | RuntimeExpr::Gt(a, b) | RuntimeExpr::Lte(a, b)
            | RuntimeExpr::Gte(a, b) | RuntimeExpr::And(a, b) | RuntimeExpr::Or(a, b)) => vec![*a, *b],
        Some(RuntimeExpr::Not(a)) => vec![*a],
        Some(RuntimeExpr::List(elems) | RuntimeExpr::Tuple(elems)) => elems.clone(),
        // Chunk 11: lambdas inside handler scope have op_name in their
        // captured-var set. Rewriting in the body causes them to close over
        // the lifted __op fn + any prepended capture variables.
        Some(RuntimeExpr::Lambda { body, .. }) => {
            let body = *body;
            rewrite_call_stmt_with_prepend(ast, body, op_name, new_fn, prepend);
            return;
        }
        Some(RuntimeExpr::DotCall { object, args, .. }) => {
            let mut v = vec![*object];
            v.extend(args.iter().copied());
            v
        }
        Some(RuntimeExpr::ClassLiteral { fields, .. }) => fields.iter().map(|(_, id)| *id).collect(),
        _ => return,
    };
    for c in children { rewrite_call_expr_with_prepend(ast, c, op_name, new_fn, prepend); }
}
