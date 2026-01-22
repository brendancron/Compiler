use super::dependency_scheduler::*;
use crate::frontend::id_provider::*;
use crate::frontend::meta_ast::*;
use crate::runtime::environment::*;
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
    dependency_scheduler: &'a mut DependencyScheduler<Dependency, Event>,
    dependency_queue: VecDeque<Dependency>,
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
pub enum Event {
    DependencyChain(Dependency),
    MetaExec(AstId),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Dependency {
    NodeDone(AstId),
}

#[derive(Debug)]
pub enum WorkItem {
    LowerExpr { meta_id: AstId, runtime_id: AstId },
    LowerStmt { meta_id: AstId, runtime_id: AstId },
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

    pub fn queue_expr(&mut self, id_provider: &mut IdProvider, meta_id: AstId) -> AstId {
        let runtime_id = id_provider.next();
        let item = WorkItem::LowerExpr {
            meta_id,
            runtime_id,
        };
        self.queue(item);
        runtime_id
    }

    pub fn queue_stmt(&mut self, id_provider: &mut IdProvider, meta_id: AstId) -> AstId {
        let runtime_id = id_provider.next();
        let item = WorkItem::LowerStmt {
            meta_id,
            runtime_id,
        };
        self.queue(item);
        runtime_id
    }

    pub fn next(&mut self) -> Option<WorkItem> {
        self.queue.pop_front()
    }
}

pub fn insert_node<W: Write>(
    node_id: AstId,
    node: RuntimeNode,
    children: Vec<AstId>,
    ctx: &mut MetaProcessContext<W>,
) {
    match node {
        RuntimeNode::Expr(expr) => ctx.runtime_ast.insert_expr(node_id, expr),
        RuntimeNode::Stmt(stmt) => ctx.runtime_ast.insert_stmt(node_id, stmt),
    }
    ctx.dependency_scheduler.add_task(
        &children
            .iter()
            .map(|&c| Dependency::NodeDone(c))
            .collect::<Vec<_>>(),
        Event::DependencyChain(Dependency::NodeDone(node_id)),
    );
}

pub fn insert_leaf<W: Write>(node_id: AstId, node: RuntimeNode, ctx: &mut MetaProcessContext<W>) {
    match node {
        RuntimeNode::Expr(expr) => ctx.runtime_ast.insert_expr(node_id, expr),
        RuntimeNode::Stmt(stmt) => ctx.runtime_ast.insert_stmt(node_id, stmt),
    }
    ctx.dependency_queue
        .push_back(Dependency::NodeDone(node_id));
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
            insert_leaf(runtime_expr_id, RuntimeNode::Expr(expr), ctx);
        }
        MetaExpr::String(s) => {
            let expr = RuntimeExpr::String(s.clone());
            insert_leaf(runtime_expr_id, RuntimeNode::Expr(expr), ctx);
        }
        MetaExpr::Bool(b) => {
            let expr = RuntimeExpr::Bool(*b);
            insert_leaf(runtime_expr_id, RuntimeNode::Expr(expr), ctx);
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

            insert_leaf(runtime_expr_id, RuntimeNode::Expr(expr), ctx);
        }

        //MetaExpr::Variable(name) => match ctx.env.borrow().get(name) {
        //    Some(x) => value_to_literal(x, ctx),
        //    None => Ok(RuntimeExpr::Variable(name.clone())),
        //},
        //TODO replace the value to lit inlining
        MetaExpr::Variable(name) => {
            let expr = RuntimeExpr::Variable(name.clone());
            insert_leaf(runtime_expr_id, RuntimeNode::Expr(expr), ctx);
        }

        MetaExpr::List(exprs) => {
            let mut ids = Vec::with_capacity(exprs.len());
            for e in exprs {
                let id = work_queue.queue_expr(ctx.id_provider, *e);
                ids.push(id);
            }

            let expr = RuntimeExpr::List(ids.clone());
            insert_node(runtime_expr_id, RuntimeNode::Expr(expr), ids, ctx);
        }

        MetaExpr::Add(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Add(a_id, b_id);
            insert_node(
                runtime_expr_id,
                RuntimeNode::Expr(expr),
                vec![a_id, b_id],
                ctx,
            );
        }

        MetaExpr::Sub(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Sub(a_id, b_id);
            insert_node(
                runtime_expr_id,
                RuntimeNode::Expr(expr),
                vec![a_id, b_id],
                ctx,
            );
        }

        MetaExpr::Mult(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Mult(a_id, b_id);
            insert_node(
                runtime_expr_id,
                RuntimeNode::Expr(expr),
                vec![a_id, b_id],
                ctx,
            );
        }

        MetaExpr::Div(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Div(a_id, b_id);
            insert_node(
                runtime_expr_id,
                RuntimeNode::Expr(expr),
                vec![a_id, b_id],
                ctx,
            );
        }

        MetaExpr::Equals(a, b) => {
            let a_id = work_queue.queue_expr(ctx.id_provider, *a);
            let b_id = work_queue.queue_expr(ctx.id_provider, *b);
            let expr = RuntimeExpr::Equals(a_id, b_id);
            insert_node(
                runtime_expr_id,
                RuntimeNode::Expr(expr),
                vec![a_id, b_id],
                ctx,
            );
        }

        MetaExpr::Call { callee, args } => {
            let mut out_args = Vec::with_capacity(args.len());

            for meta_arg in args {
                let arg_id = work_queue.queue_expr(ctx.id_provider, *meta_arg);
                out_args.push(arg_id);
            }

            let expr = RuntimeExpr::Call {
                callee: callee.clone(),
                args: out_args.clone(),
            };

            insert_node(runtime_expr_id, RuntimeNode::Expr(expr), out_args, ctx);
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
            insert_leaf(runtime_expr_id, RuntimeNode::Expr(type_expr), ctx);
        }

        MetaExpr::Embed(file_path) => {
            let expr = RuntimeExpr::String(file_path.clone());
            insert_leaf(runtime_expr_id, RuntimeNode::Expr(expr), ctx);
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
            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), vec![expr_id], ctx);
        }

        MetaStmt::VarDecl { name, expr } => {
            let expr_id = work_queue.queue_expr(ctx.id_provider, *expr);
            let stmt = RuntimeStmt::VarDecl {
                name: name.clone(),
                expr: expr_id,
            };
            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), vec![expr_id], ctx);
        }

        MetaStmt::Print(expr) => {
            let expr_id = work_queue.queue_expr(ctx.id_provider, *expr);
            let stmt = RuntimeStmt::Print(expr_id);
            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), vec![expr_id], ctx);
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

            let mut children = vec![cond_id, body_id];
            if let Some(eid) = else_id {
                children.push(eid);
            }

            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), children, ctx);
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

            insert_node(
                runtime_stmt_id,
                RuntimeNode::Stmt(stmt),
                vec![iterable_id, body_id],
                ctx,
            );
        }

        MetaStmt::Block(stmts) => {
            let mut child_ids = Vec::with_capacity(stmts.len());

            for meta_stmt in stmts {
                let stmt_id = work_queue.queue_stmt(ctx.id_provider, *meta_stmt);
                child_ids.push(stmt_id);
            }

            let stmt = RuntimeStmt::Block(child_ids.clone());
            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), child_ids, ctx);
        }

        MetaStmt::FnDecl { name, params, body } => {
            let body_id = work_queue.queue_stmt(ctx.id_provider, *body);

            let stmt = RuntimeStmt::FnDecl {
                name: name.clone(),
                params: params.clone(),
                body: body_id,
            };

            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), vec![body_id], ctx);
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

            let mut children = Vec::new();
            if let Some(id) = expr_id {
                children.push(id);
            }

            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), children, ctx);
        }

        MetaStmt::Gen(stmts) => {
            let child_ids: Vec<_> = stmts
                .iter()
                .map(|s| work_queue.queue_stmt(ctx.id_provider, *s))
                .collect();

            let stmt = RuntimeStmt::Gen(child_ids.clone());

            insert_node(runtime_stmt_id, RuntimeNode::Stmt(stmt), child_ids, ctx);
        }

        MetaStmt::MetaBlock(parsed_stmt) => {
            let body_id = work_queue.queue_stmt(ctx.id_provider, *parsed_stmt);

            ctx.dependency_scheduler
                .add_task(&[Dependency::NodeDone(body_id)], Event::MetaExec(body_id));
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

pub fn process<W: Write>(
    ast: &MetaAst,
    out: &mut W,
    root_stmts: &Vec<AstId>,
) -> Result<RuntimeAst, MetaProcessError> {
    let mut runtime_ast = RuntimeAst::new();
    let mut ctx = MetaProcessContext {
        meta_ast: ast,
        runtime_ast: &mut runtime_ast,
        out,
        id_provider: &mut IdProvider::new(),
        dependency_scheduler: &mut DependencyScheduler::new(),
        dependency_queue: VecDeque::new(),
    };

    let mut work_queue = WorkQueue::new();

    for stmt in root_stmts {
        let runtime_id = work_queue.queue_stmt(ctx.id_provider, *stmt);
        ctx.runtime_ast.sem_root_stmts.push(runtime_id);
    }

    while let Some(work_item) = work_queue.next() {
        println!("{:?}", work_item);
        match work_item {
            WorkItem::LowerExpr {
                runtime_id,
                meta_id,
            } => {
                process_expr(meta_id, runtime_id, &mut work_queue, &mut ctx)?;
            }

            WorkItem::LowerStmt {
                runtime_id,
                meta_id,
            } => {
                process_stmt(meta_id, runtime_id, &mut work_queue, &mut ctx)?;
            }
        }

        println!("{:?}", ctx.dependency_scheduler);

        while let Some(dep) = ctx.dependency_queue.pop_front() {
            println!("dependency completed: {:?}", dep);
            let events = ctx.dependency_scheduler.resolve_dependency(dep);
            for event in events {
                println!("event emitted: {:?}", event);
                match event {
                    Event::DependencyChain(dependency) => {
                        ctx.dependency_queue.push_back(dependency);
                    }

                    Event::MetaExec(ast_id) => {
                        let stmts = vec![ast_id];
                        eval(
                            ctx.runtime_ast,
                            &stmts,
                            Environment::new(),
                            &mut None,
                            ctx.out,
                        )?;
                    }
                }
            }
        }
    }

    Ok(runtime_ast)
}
