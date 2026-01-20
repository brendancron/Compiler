use crate::frontend::id_provider::*;
use crate::frontend::meta_ast::*;
use crate::runtime::interpreter::*;
use crate::runtime::value::Value;
use crate::semantics::meta::runtime_ast::*;
use std::collections::VecDeque;
use std::io::Write;

pub struct MetaProcessContext<'a, W: Write> {
    meta_ast: &'a MetaAst,
    runtime_ast: &'a mut RuntimeAst,
    out: &'a mut W,
    id_provider: &'a mut IdProvider,
}

#[derive(Debug)]
pub enum MetaProcessError {
    ExprNotFound(AstId),
    StmtNotFound(AstId),
    EmbedFailed { path: String, error: String },
    UnknownType(String),
    Unimplemented(String),
    Eval(EvalError),
}

impl From<EvalError> for MetaProcessError {
    fn from(e: EvalError) -> Self {
        MetaProcessError::Eval(e)
    }
}

pub struct MetaContext {
    pub emitted: Vec<RuntimeStmt>,
}

#[derive(Debug)]
pub struct WorkItem {
    runtime_id: usize,
    node: MetaAstNode,
}

impl WorkItem {
    fn from_expr(id_provider: &mut IdProvider, meta_expr_id: AstId) -> Self {
        Self {
            runtime_id: id_provider.next(),
            node: MetaAstNode::Expr(meta_expr_id),
        }
    }

    fn from_stmt(id_provider: &mut IdProvider, meta_stmt_id: AstId) -> Self {
        Self {
            runtime_id: id_provider.next(),
            node: MetaAstNode::Stmt(meta_stmt_id),
        }
    }
}

pub struct WorkQueue {
    queue: VecDeque<WorkItem>,
}

impl WorkQueue {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    fn queue(&mut self, item: WorkItem) {
        self.queue.push_back(item);
    }

    pub fn queue_expr(&mut self, id_provider: &mut IdProvider, id: AstId) -> AstId {
        let item = WorkItem::from_expr(id_provider, id);
        let id = item.runtime_id;
        self.queue(item);
        id
    }

    pub fn queue_stmt(&mut self, id_provider: &mut IdProvider, id: AstId) -> AstId {
        let item = WorkItem::from_stmt(id_provider, id);
        let id = item.runtime_id;
        self.queue(item);
        id
    }

    pub fn next(&mut self) -> Option<WorkItem> {
        self.queue.pop_front()
    }
}

pub fn process_expr<W: Write>(
    meta_expr_id: AstId,
    runtime_expr_id: AstId,
    work_queue: &mut WorkQueue,
    ctx: &mut MetaProcessContext<W>,
) -> Result<(), MetaProcessError> {
    let meta_expr = ctx
        .meta_ast
        .get_expr(meta_expr_id)
        .ok_or(MetaProcessError::ExprNotFound(meta_expr_id))?;
    match meta_expr {
        MetaExpr::Int(i) => {
            let expr = RuntimeExpr::Int(*i);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }
        MetaExpr::String(s) => {
            let expr = RuntimeExpr::String(s.clone());
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }
        MetaExpr::Bool(b) => {
            let expr = RuntimeExpr::Bool(*b);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::StructLiteral { type_name, fields } => {
            let mut out_fields = Vec::with_capacity(fields.len());

            for (name, meta_expr_id) in fields {
                let field_expr_id = work_queue.queue_expr(ctx.id_provider, *meta_expr_id);
                out_fields.push((name.clone(), field_expr_id));
            }

            let expr = RuntimeExpr::StructLiteral {
                type_name: type_name.clone(),
                fields: out_fields,
            };

            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        //MetaExpr::Variable(name) => match ctx.env.borrow().get(name) {
        //    Some(x) => value_to_literal(x, ctx),
        //    None => Ok(RuntimeExpr::Variable(name.clone())),
        //},
        //TODO replace the value to lit inlining
        MetaExpr::Variable(name) => {
            let expr = RuntimeExpr::Variable(name.clone());
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::List(exprs) => {
            let mut ids = Vec::with_capacity(exprs.len());
            for e in exprs {
                let id = work_queue.queue_expr(ctx.id_provider, *e);
                ids.push(id);
            }

            let expr = RuntimeExpr::List(ids);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::Add(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Add(a_id, b_id);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::Sub(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Sub(a_id, b_id);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::Mult(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Mult(a_id, b_id);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::Div(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Div(a_id, b_id);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::Equals(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Equals(a_id, b_id);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        MetaExpr::Call { callee, args } => {
            let mut out_args = Vec::with_capacity(args.len());

            for meta_arg in args {
                let arg_id = work_queue.queue_expr(ctx.id_provider, *meta_arg);
                out_args.push(arg_id);
            }

            let expr = RuntimeExpr::Call {
                callee: callee.clone(),
                args: out_args,
            };

            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }

        //match ctx.env.borrow().get(&callee) {
        //    Some(_) => {
        //        let val = interpreter::eval_expr(
        //            &call_expr,
        //            ctx.env.clone(),
        //            ctx.decls,
        //            &mut None,
        //            ctx.out,
        //        )?;
        //        value_to_literal(val)
        //    }
        //    None => Ok(call_expr),
        //}
        MetaExpr::Typeof(ident) => {
            //let def = ctx
            //    .decls
            //    .get_struct(ident)
            //    .ok_or_else(|| MetaProcessError::UnknownType(ident.clone()))?;

            let type_expr = RuntimeExpr::String(ident.clone());
            ctx.runtime_ast.insert_expr(runtime_expr_id, type_expr);
        }

        MetaExpr::Embed(file_path) => {
            let expr = RuntimeExpr::String(file_path.clone());
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
        }
    };
    Ok(())
}

pub fn process_stmt<W: Write>(
    meta_stmt_id: AstId,
    runtime_stmt_id: AstId,
    work_queue: &mut WorkQueue,
    ctx: &mut MetaProcessContext<W>,
) -> Result<(), MetaProcessError> {
    let meta_stmt = ctx
        .meta_ast
        .get_stmt(meta_stmt_id)
        .ok_or(MetaProcessError::StmtNotFound(meta_stmt_id))?;
    match meta_stmt {
        MetaStmt::ExprStmt(expr) => {
            let expr_id = work_queue.queue_expr(ctx.id_provider, *expr);
            let stmt = RuntimeStmt::ExprStmt(expr_id);
            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::VarDecl { name, expr } => {
            let expr_id = work_queue.queue_expr(ctx.id_provider, *expr);
            let stmt = RuntimeStmt::VarDecl {
                name: name.clone(),
                expr: expr_id,
            };
            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::Print(expr) => {
            let expr_id = work_queue.queue_expr(ctx.id_provider, *expr);
            let stmt = RuntimeStmt::Print(expr_id);
            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::If {
            cond,
            body,
            else_branch,
        } => {
            let cond_id = work_queue.queue_expr(ctx.id_provider, *cond);
            let body_id = work_queue.queue_stmt(ctx.id_provider, *body);

            let else_id = else_branch
                .as_ref()
                .map(|s| work_queue.queue_stmt(ctx.id_provider, *s));

            let stmt = RuntimeStmt::If {
                cond: cond_id,
                body: body_id,
                else_branch: else_id,
            };

            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::ForEach {
            var,
            iterable,
            body,
        } => {
            let iterable_id = work_queue.queue_expr(ctx.id_provider, *iterable);
            let body_id = work_queue.queue_stmt(ctx.id_provider, *body);

            let stmt = RuntimeStmt::ForEach {
                var: var.clone(),
                iterable: iterable_id,
                body: body_id,
            };

            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::Block(stmts) => {
            let mut child_ids = Vec::with_capacity(stmts.len());

            for meta_stmt in stmts {
                let stmt_id = work_queue.queue_stmt(ctx.id_provider, *meta_stmt);
                child_ids.push(stmt_id);
            }

            let stmt = RuntimeStmt::Block(child_ids);
            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::FnDecl { name, params, body } => {
            let body_id = work_queue.queue_stmt(ctx.id_provider, *body);

            let stmt = RuntimeStmt::FnDecl {
                name: name.clone(),
                params: params.clone(),
                body: body_id,
            };

            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::StructDecl { name, fields } => {
            //ctx.decls.define_struct(
            //    name.clone(),
            //    StructDef {
            //        fields: fields.clone(),
            //    },
            //);
        }

        MetaStmt::Return(expr) => {
            let expr_id = expr.map(|e| work_queue.queue_expr(ctx.id_provider, e));

            let stmt = RuntimeStmt::Return(expr_id);
            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::Gen(stmts) => {
            let mut child_ids = Vec::with_capacity(stmts.len());

            for meta_stmt in stmts {
                let id = work_queue.queue_stmt(ctx.id_provider, *meta_stmt);
                child_ids.push(id);
            }

            let stmt = RuntimeStmt::Gen(child_ids);
            ctx.runtime_ast.insert_stmt(runtime_stmt_id, stmt);
        }

        MetaStmt::MetaBlock(parsed_stmt) => {
            work_queue.queue_expr(ctx.id_provider, *parsed_stmt);
            // TODO we need to somehow queue this expr id in such a way that eval gets called as a
            // callback

            //eval(
            //    ctx.runtime_ast,
            //    &processed_code,
            //    Environment::new(),
            //    &mut None,
            //    ctx.out,
            //)?;
        }

        MetaStmt::Import(_mod_name) => {}
    };
    Ok(())
}

pub fn value_to_literal<W: Write>(
    val: Value,
    runtime_expr_id: AstId,
    ctx: MetaProcessContext<W>,
) -> Result<(), MetaProcessError> {
    match val {
        Value::Int(n) => {
            let expr = RuntimeExpr::Int(n);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
            Ok(())
        }
        Value::String(s) => {
            let expr = RuntimeExpr::String(s);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
            Ok(())
        }
        Value::Bool(b) => {
            let expr = RuntimeExpr::Bool(b);
            ctx.runtime_ast.insert_expr(runtime_expr_id, expr);
            Ok(())
        }
        Value::Unit => Err(MetaProcessError::Unimplemented(
            "Unit has no literal representation".to_string(),
        )),
        _ => Err(MetaProcessError::Unimplemented(
            "non-primitive value not supported yet".to_string(),
        )),
    }
}

pub fn process<W: Write>(ast: &MetaAst, out: &mut W) -> Result<RuntimeAst, MetaProcessError> {
    let mut runtime_ast = RuntimeAst::new();
    let mut ctx = MetaProcessContext {
        meta_ast: ast,
        runtime_ast: &mut runtime_ast,
        out,
        id_provider: &mut IdProvider::new(),
    };

    let mut work_queue = WorkQueue::new();

    for stmt in &ast.sem_root_stmts {
        let runtime_id = work_queue.queue_stmt(ctx.id_provider, *stmt);
        ctx.runtime_ast.sem_root_stmts.push(runtime_id);
    }

    while let Some(work_item) = work_queue.next() {
        match work_item.node {
            MetaAstNode::Expr(meta_id) => {
                process_expr(meta_id, work_item.runtime_id, &mut work_queue, &mut ctx)?;
            }
            MetaAstNode::Stmt(meta_id) => {
                process_stmt(meta_id, work_item.runtime_id, &mut work_queue, &mut ctx)?;
            }
        }
    }

    Ok(runtime_ast)
}
