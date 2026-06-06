use std::collections::{HashMap, HashSet};
use crate::semantics::meta::runtime_ast::*;
use crate::semantics::types::types::{PrimitiveType, Type, TypeVar};
use crate::util::node_id::RuntimeNodeId;

fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Primitive(PrimitiveType::Int) => "int".to_string(),
        Type::Primitive(PrimitiveType::String) => "str".to_string(),
        Type::Primitive(PrimitiveType::Bool) => "bool".to_string(),
        Type::Primitive(PrimitiveType::Unit) => "unit".to_string(),
        Type::Var(TypeVar { id }) => format!("t{id}"),
        Type::Func { .. } => "fn".to_string(),
        Type::Record(fields) => {
            let keys = fields.keys().cloned().collect::<Vec<_>>().join("_");
            format!("rec_{keys}")
        }
        Type::Class { name, .. } => name.clone(),
        Type::Tuple(items) => {
            let inner = items.iter().map(mangle_type).collect::<Vec<_>>().join("_");
            format!("tuple_{inner}")
        }
        Type::Slice(elem) => format!("slice_{}", mangle_type(elem)),
        Type::Enum(name) => name.clone(),
        Type::App(name, args) => {
            let inner = args.iter().map(mangle_type).collect::<Vec<_>>().join("_");
            format!("{name}_{inner}")
        }
    }
}

// ── ID-remapping clone helpers ──────────────────────────────────────────────

fn clone_expr(
    ast: &RuntimeAst,
    expr_id: RuntimeNodeId,
    next_id: &mut usize,
    new_stmts: &mut HashMap<RuntimeNodeId, RuntimeStmt>,
    new_exprs: &mut HashMap<RuntimeNodeId, RuntimeExpr>,
    stmt_map: &mut HashMap<RuntimeNodeId, RuntimeNodeId>,
    expr_map: &mut HashMap<RuntimeNodeId, RuntimeNodeId>,
) -> RuntimeNodeId {
    if let Some(&mapped) = expr_map.get(&expr_id) {
        return mapped;
    }
    let new_id = RuntimeNodeId(*next_id);
    *next_id += 1;
    expr_map.insert(expr_id, new_id);

    let expr = ast.get_expr(expr_id).expect("invalid expr id during monomorphize clone").clone();

    macro_rules! ce {
        ($id:expr) => { clone_expr(ast, $id, next_id, new_stmts, new_exprs, stmt_map, expr_map) }
    }

    let new_expr = match expr {
        RuntimeExpr::Int(_) | RuntimeExpr::String(_) | RuntimeExpr::Bool(_) | RuntimeExpr::Variable(_) => expr,
        RuntimeExpr::Add(a, b) => RuntimeExpr::Add(ce!(a), ce!(b)),
        RuntimeExpr::Sub(a, b) => RuntimeExpr::Sub(ce!(a), ce!(b)),
        RuntimeExpr::Mult(a, b) => RuntimeExpr::Mult(ce!(a), ce!(b)),
        RuntimeExpr::Div(a, b) => RuntimeExpr::Div(ce!(a), ce!(b)),
        RuntimeExpr::Mod(a, b) => RuntimeExpr::Mod(ce!(a), ce!(b)),
        RuntimeExpr::Equals(a, b) => RuntimeExpr::Equals(ce!(a), ce!(b)),
        RuntimeExpr::NotEquals(a, b) => RuntimeExpr::NotEquals(ce!(a), ce!(b)),
        RuntimeExpr::Lt(a, b) => RuntimeExpr::Lt(ce!(a), ce!(b)),
        RuntimeExpr::Gt(a, b) => RuntimeExpr::Gt(ce!(a), ce!(b)),
        RuntimeExpr::Lte(a, b) => RuntimeExpr::Lte(ce!(a), ce!(b)),
        RuntimeExpr::Gte(a, b) => RuntimeExpr::Gte(ce!(a), ce!(b)),
        RuntimeExpr::And(a, b) => RuntimeExpr::And(ce!(a), ce!(b)),
        RuntimeExpr::Or(a, b) => RuntimeExpr::Or(ce!(a), ce!(b)),
        RuntimeExpr::Not(a) => RuntimeExpr::Not(ce!(a)),
        RuntimeExpr::List(items) => RuntimeExpr::List(items.iter().map(|&i| ce!(i)).collect()),
        RuntimeExpr::Tuple(items) => RuntimeExpr::Tuple(items.iter().map(|&i| ce!(i)).collect()),
        RuntimeExpr::TupleIndex { object, index } => RuntimeExpr::TupleIndex { object: ce!(object), index },
        RuntimeExpr::Index { object, index } => RuntimeExpr::Index { object: ce!(object), index: ce!(index) },
        RuntimeExpr::DotAccess { object, field } => RuntimeExpr::DotAccess { object: ce!(object), field },
        RuntimeExpr::DotCall { object, method, args } => RuntimeExpr::DotCall {
            object: ce!(object),
            method,
            args: args.iter().map(|&a| ce!(a)).collect(),
        },
        RuntimeExpr::Call { callee, args } => RuntimeExpr::Call {
            callee,
            args: args.iter().map(|&a| ce!(a)).collect(),
        },
        RuntimeExpr::ClassLiteral { type_name, fields } => RuntimeExpr::ClassLiteral {
            type_name,
            fields: fields.iter().map(|(n, id)| (n.clone(), ce!(*id))).collect(),
        },
        RuntimeExpr::EnumConstructor { enum_name, variant, payload } => RuntimeExpr::EnumConstructor {
            enum_name,
            variant,
            payload: match payload {
                RuntimeConstructorPayload::Unit => RuntimeConstructorPayload::Unit,
                RuntimeConstructorPayload::Tuple(ids) => RuntimeConstructorPayload::Tuple(ids.iter().map(|&i| ce!(i)).collect()),
                RuntimeConstructorPayload::Struct(fields) => RuntimeConstructorPayload::Struct(
                    fields.iter().map(|(n, id)| (n.clone(), ce!(*id))).collect(),
                ),
            },
        },
        RuntimeExpr::SliceRange { object, start, end } => RuntimeExpr::SliceRange {
            object: ce!(object),
            start: start.map(|s| ce!(s)),
            end: end.map(|e| ce!(e)),
        },
        RuntimeExpr::Lambda { params, body } => RuntimeExpr::Lambda {
            params,
            body: clone_stmt(ast, body, next_id, new_stmts, new_exprs, stmt_map, expr_map),
        },
        RuntimeExpr::Unit => RuntimeExpr::Unit,
        RuntimeExpr::ResumeExpr(opt) => RuntimeExpr::ResumeExpr(
            opt.map(|id| clone_expr(ast, id, next_id, new_stmts, new_exprs, stmt_map, expr_map)),
        ),
    };

    new_exprs.insert(new_id, new_expr);
    new_id
}

fn clone_stmt(
    ast: &RuntimeAst,
    stmt_id: RuntimeNodeId,
    next_id: &mut usize,
    new_stmts: &mut HashMap<RuntimeNodeId, RuntimeStmt>,
    new_exprs: &mut HashMap<RuntimeNodeId, RuntimeExpr>,
    stmt_map: &mut HashMap<RuntimeNodeId, RuntimeNodeId>,
    expr_map: &mut HashMap<RuntimeNodeId, RuntimeNodeId>,
) -> RuntimeNodeId {
    if let Some(&mapped) = stmt_map.get(&stmt_id) {
        return mapped;
    }
    let new_id = RuntimeNodeId(*next_id);
    *next_id += 1;
    stmt_map.insert(stmt_id, new_id);

    let stmt = ast.get_stmt(stmt_id).expect("invalid stmt id during monomorphize clone").clone();

    macro_rules! cs {
        ($id:expr) => { clone_stmt(ast, $id, next_id, new_stmts, new_exprs, stmt_map, expr_map) }
    }
    macro_rules! ce {
        ($id:expr) => { clone_expr(ast, $id, next_id, new_stmts, new_exprs, stmt_map, expr_map) }
    }

    let new_stmt = match stmt {
        RuntimeStmt::Block(children) => RuntimeStmt::Block(children.iter().map(|&c| cs!(c)).collect()),
        RuntimeStmt::Return(opt_e) => RuntimeStmt::Return(opt_e.map(|e| ce!(e))),
        RuntimeStmt::ExprStmt(e) => RuntimeStmt::ExprStmt(ce!(e)),
        RuntimeStmt::Print(e) => RuntimeStmt::Print(ce!(e)),
        RuntimeStmt::VarDecl { name, expr } => RuntimeStmt::VarDecl { name, expr: ce!(expr) },
        RuntimeStmt::Assign { name, expr } => RuntimeStmt::Assign { name, expr: ce!(expr) },
        RuntimeStmt::IndexAssign { name, indices, expr } => RuntimeStmt::IndexAssign {
            name,
            indices: indices.iter().map(|&i| ce!(i)).collect(),
            expr: ce!(expr),
        },
        RuntimeStmt::FnDecl { name, params, type_params, body } => RuntimeStmt::FnDecl {
            name,
            params,
            type_params,
            body: cs!(body),
        },
        RuntimeStmt::If { cond, body, else_branch } => RuntimeStmt::If {
            cond: ce!(cond),
            body: cs!(body),
            else_branch: else_branch.map(|e| cs!(e)),
        },
        RuntimeStmt::WhileLoop { cond, body } => RuntimeStmt::WhileLoop {
            cond: ce!(cond),
            body: cs!(body),
        },
        RuntimeStmt::ForEach { var, iterable, body } => RuntimeStmt::ForEach {
            var,
            iterable: ce!(iterable),
            body: cs!(body),
        },
        RuntimeStmt::Match { scrutinee, arms } => RuntimeStmt::Match {
            scrutinee: ce!(scrutinee),
            arms: arms.iter().map(|arm| RuntimeMatchArm {
                pattern: arm.pattern.clone(),
                body: cs!(arm.body),
            }).collect(),
        },
        other => other,
    };

    new_stmts.insert(new_id, new_stmt);
    new_id
}

// ── Main monomorphization pass ───────────────────────────────────────────────

pub fn monomorphize(ast: &mut RuntimeAst, type_map: &HashMap<RuntimeNodeId, Type>) {
    // Detect monomorphization candidates by scanning call sites: a function is
    // polymorphic when it's called with two distinct concrete arg-type tuples.
    // (Type-map signatures on the FnDecl can be misleading — Phase-2 records
    // a single concrete type even for genuinely polymorphic functions.)
    // Class constructors share the class name with a ClassDecl. Skip them —
    // their body is `return Class { f1: f1, ... }` which produces the same
    // heap layout for any T, so monomorphization would just bloat the IR.
    let class_names: std::collections::HashSet<String> = ast.stmts.values()
        .filter_map(|s| if let RuntimeStmt::ClassDecl { name, .. } = s { Some(name.clone()) } else { None })
        .collect();

    let fn_decls: HashMap<String, RuntimeNodeId> = ast.stmts.iter()
        .filter_map(|(&id, stmt)| {
            if let RuntimeStmt::FnDecl { name, type_params, .. } = stmt {
                if class_names.contains(name) { return None; }
                if !type_params.is_empty() {
                    return Some((name.clone(), id));
                }
                // Untyped: candidate if its sig in type_map has any var, OR if
                // it has no entry yet — both cases are confirmed below by
                // checking for multiple distinct concrete call sites.
                return Some((name.clone(), id));
            }
            None
        })
        .collect();

    if fn_decls.is_empty() { return; }

    // Build a name → resolved-Type map for top-level VarDecls so a Variable
    // arg whose Phase-2 type wasn't pinned can fall back to its initializer
    // type. Picks up cases like `var a = Article(...); notify(a);` where
    // `a`'s Variable expr has an unresolved Type::Var in type_map but the
    // VarDecl's initializer expr has a concrete Type::Class.
    let mut var_decl_types: HashMap<String, Type> = HashMap::new();
    for stmt in ast.stmts.values() {
        if let RuntimeStmt::VarDecl { name, expr } = stmt {
            // First try the recorded type_map entry; fall back to the
            // initializer's syntactic shape (Call → callee return type).
            if let Some(t) = type_map.get(expr) {
                if !t.contains_var() {
                    var_decl_types.insert(name.clone(), t.clone());
                    continue;
                }
            }
            if let Some(RuntimeExpr::Call { callee, .. }) = ast.get_expr(*expr) {
                // Synth class constructors all return `Type::Enum(class_name)`
                // (Type::Class isn't used for nominals here); promote.
                if fn_decls.contains_key(callee.as_str()) || callee.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    var_decl_types.insert(name.clone(), Type::Enum(callee.clone()));
                }
            }
        }
    }

    // Group concrete arg-types per callee.
    let mut callee_arg_tuples: HashMap<String, std::collections::BTreeSet<Vec<String>>> = HashMap::new();
    for expr in ast.exprs.values() {
        if let RuntimeExpr::Call { callee, args } = expr {
            if !fn_decls.contains_key(callee.as_str()) { continue; }
            let arg_types: Vec<Type> = args.iter()
                .map(|&aid| {
                    let primary = type_map.get(&aid).cloned()
                        .unwrap_or(Type::Var(TypeVar { id: usize::MAX }));
                    if !primary.contains_var() { return primary; }
                    // Fallback: if arg is a bare Variable, use its VarDecl's
                    // resolved initializer type.
                    if let Some(RuntimeExpr::Variable(vname)) = ast.get_expr(aid) {
                        if let Some(t) = var_decl_types.get(vname) {
                            return t.clone();
                        }
                    }
                    primary
                })
                .collect();
            if arg_types.iter().any(|t| t.contains_var()) { continue; }
            let key: Vec<String> = arg_types.iter().map(mangle_type).collect();
            callee_arg_tuples.entry(callee.clone()).or_default().insert(key);
        }
    }

    // Helper: does `body_id` reference `self_name` anywhere?
    fn references_name(ast: &RuntimeAst, body_id: RuntimeNodeId, self_name: &str) -> bool {
        fn s(ast: &RuntimeAst, id: RuntimeNodeId, n: &str) -> bool {
            match ast.get_stmt(id) {
                Some(RuntimeStmt::Block(c)) => c.iter().any(|&i| s(ast, i, n)),
                Some(RuntimeStmt::ExprStmt(e))
                | Some(RuntimeStmt::Print(e))
                | Some(RuntimeStmt::Return(Some(e)))
                | Some(RuntimeStmt::VarDecl { expr: e, .. })
                | Some(RuntimeStmt::Assign { expr: e, .. }) => e_ref(ast, *e, n),
                Some(RuntimeStmt::If { cond, body, else_branch }) => {
                    e_ref(ast, *cond, n) || s(ast, *body, n)
                        || else_branch.map(|e| s(ast, e, n)).unwrap_or(false)
                }
                Some(RuntimeStmt::WhileLoop { cond, body }) => e_ref(ast, *cond, n) || s(ast, *body, n),
                Some(RuntimeStmt::ForEach { iterable, body, .. }) => e_ref(ast, *iterable, n) || s(ast, *body, n),
                Some(RuntimeStmt::Match { scrutinee, arms }) =>
                    e_ref(ast, *scrutinee, n) || arms.iter().any(|a| s(ast, a.body, n)),
                _ => false,
            }
        }
        fn e_ref(ast: &RuntimeAst, id: RuntimeNodeId, n: &str) -> bool {
            match ast.get_expr(id) {
                Some(RuntimeExpr::Call { callee, args }) =>
                    callee == n || args.iter().any(|&a| e_ref(ast, a, n)),
                Some(RuntimeExpr::DotCall { object, args, .. }) =>
                    e_ref(ast, *object, n) || args.iter().any(|&a| e_ref(ast, a, n)),
                Some(RuntimeExpr::DotAccess { object, .. }) => e_ref(ast, *object, n),
                Some(RuntimeExpr::Add(a,b)|RuntimeExpr::Sub(a,b)|RuntimeExpr::Mult(a,b)|RuntimeExpr::Div(a,b)|RuntimeExpr::Mod(a,b)
                    |RuntimeExpr::Equals(a,b)|RuntimeExpr::NotEquals(a,b)|RuntimeExpr::Lt(a,b)|RuntimeExpr::Gt(a,b)
                    |RuntimeExpr::Lte(a,b)|RuntimeExpr::Gte(a,b)|RuntimeExpr::And(a,b)|RuntimeExpr::Or(a,b))
                    => e_ref(ast, *a, n) || e_ref(ast, *b, n),
                Some(RuntimeExpr::Not(a)) => e_ref(ast, *a, n),
                Some(RuntimeExpr::Variable(name)) => name == n,
                _ => false,
            }
        }
        s(ast, body_id, self_name)
    }

    // A function is genuinely polymorphic only if it has ≥2 distinct concrete
    // call shapes. (Single-shape callees stay un-renamed; nothing to do.)
    // Skip recursive generic functions — naïve per-callsite renaming would
    // mis-route the recursive call to the outer specialization.
    let generic_fns: HashMap<String, RuntimeNodeId> = fn_decls.iter()
        .filter(|(name, _)| callee_arg_tuples.get(name.as_str()).map(|s| s.len()) > Some(1))
        .filter(|(name, id)| {
            let body = if let Some(RuntimeStmt::FnDecl { body, .. }) = ast.stmts.get(id) {
                *body
            } else { return false; };
            !references_name(ast, body, name)
        })
        .map(|(n, id)| (n.clone(), *id))
        .collect();

    if generic_fns.is_empty() {
        return;
    }

    let mut next_id = ast.stmts.keys().chain(ast.exprs.keys()).map(|id| id.0).max().unwrap_or(0) + 1;

    // Each (callee, concrete-arg-tuple) → unique mangled name.
    let mut instantiations: HashMap<String, (String, RuntimeNodeId, Vec<Type>)> = HashMap::new();
    let mut call_rewrites: HashMap<RuntimeNodeId, String> = HashMap::new();

    for (&expr_id, expr) in &ast.exprs {
        if let RuntimeExpr::Call { callee, args } = expr {
            if let Some(&orig_stmt_id) = generic_fns.get(callee.as_str()) {
                let arg_types: Vec<Type> = args.iter()
                    .map(|&arg_id| {
                        let primary = type_map.get(&arg_id).cloned()
                            .unwrap_or(Type::Var(TypeVar { id: usize::MAX }));
                        if !primary.contains_var() { return primary; }
                        if let Some(RuntimeExpr::Variable(vname)) = ast.get_expr(arg_id) {
                            if let Some(t) = var_decl_types.get(vname) {
                                return t.clone();
                            }
                        }
                        primary
                    })
                    .collect();
                // Skip call sites with unresolved arg types — we can't pick
                // a specialization for those.
                if arg_types.iter().any(|t| t.contains_var()) { continue; }
                let suffix = arg_types.iter().map(mangle_type).collect::<Vec<_>>().join("__");
                let mangled = format!("{callee}__{suffix}");
                instantiations.entry(mangled.clone())
                    .or_insert_with(|| (callee.clone(), orig_stmt_id, arg_types.clone()));
                call_rewrites.insert(expr_id, mangled);
            }
        }
    }

    if instantiations.is_empty() {
        return;
    }

    let mut new_fn_stmt_ids: Vec<RuntimeNodeId> = Vec::new();
    let mut cloned_call_rewrites: HashMap<RuntimeNodeId, String> = HashMap::new();
    for (mangled_name, (_, orig_stmt_id, _arg_types)) in &instantiations {
        if let Some(RuntimeStmt::FnDecl { params, body, .. }) = ast.stmts.get(orig_stmt_id).cloned() {
            let mut stmt_map: HashMap<RuntimeNodeId, RuntimeNodeId> = HashMap::new();
            let mut expr_map: HashMap<RuntimeNodeId, RuntimeNodeId> = HashMap::new();
            let mut new_stmts: HashMap<RuntimeNodeId, RuntimeStmt> = HashMap::new();
            let mut new_exprs: HashMap<RuntimeNodeId, RuntimeExpr> = HashMap::new();

            let new_body = clone_stmt(
                ast, body, &mut next_id,
                &mut new_stmts, &mut new_exprs,
                &mut stmt_map, &mut expr_map,
            );

            for (&orig_id, &new_id) in &expr_map {
                if let Some(mangled) = call_rewrites.get(&orig_id) {
                    cloned_call_rewrites.insert(new_id, mangled.clone());
                }
            }

            for (&_orig_id, &new_id) in &expr_map {
                if !cloned_call_rewrites.contains_key(&new_id) {
                    if let Some(RuntimeExpr::Call { callee, .. }) = new_exprs.get(&new_id) {
                        if generic_fns.contains_key(callee.as_str()) {
                            cloned_call_rewrites.insert(new_id, mangled_name.clone());
                        }
                    }
                }
            }

            let fn_id = RuntimeNodeId(next_id);
            next_id += 1;
            new_stmts.insert(fn_id, RuntimeStmt::FnDecl {
                name: mangled_name.clone(),
                params,
                type_params: vec![],
                body: new_body,
            });

            ast.stmts.extend(new_stmts);
            ast.exprs.extend(new_exprs);
            new_fn_stmt_ids.push(fn_id);
        }
    }

    for (expr_id, mangled_name) in call_rewrites.iter().chain(cloned_call_rewrites.iter()) {
        if let Some(RuntimeExpr::Call { args, .. }) = ast.exprs.get(expr_id).cloned() {
            ast.exprs.insert(*expr_id, RuntimeExpr::Call {
                callee: mangled_name.clone(),
                args,
            });
        }
    }

    let generic_ids: HashSet<RuntimeNodeId> = generic_fns.values().copied().collect();
    ast.sem_root_stmts.retain(|id| !generic_ids.contains(id));
    for id in &generic_ids {
        ast.stmts.remove(id);
    }

    let mut new_roots = new_fn_stmt_ids;
    new_roots.append(&mut ast.sem_root_stmts);
    ast.sem_root_stmts = new_roots;
}
