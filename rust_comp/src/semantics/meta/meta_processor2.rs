use crate::frontend::meta_ast::*;
use crate::runtime::environment::*;
use crate::runtime::interpreter2::*;
use crate::runtime::value::Value;
use crate::semantics::meta::runtime_ast::*;
use std::io::Write;

pub struct MetaProcessContext<'a, W: Write> {
    meta_ast: &'a MetaAst,
    runtime_ast: &'a mut RuntimeAst,
    out: &'a mut W,
}

#[derive(Debug)]
pub enum MetaProcessError {
    ExprNotFound(MetaExprId),
    StmtNotFound(MetaStmtId),
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

pub fn value_to_literal<W: Write>(
    val: Value,
    ctx: MetaProcessContext<W>,
) -> Result<RuntimeExprId, MetaProcessError> {
    match val {
        Value::Int(n) => {
            let expr = RuntimeExpr::Int(n);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }
        Value::String(s) => {
            let expr = RuntimeExpr::String(s);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }
        Value::Bool(b) => {
            let expr = RuntimeExpr::Bool(b);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }
        Value::Unit => Err(MetaProcessError::Unimplemented(
            "Unit has no literal representation".to_string(),
        )),
        _ => Err(MetaProcessError::Unimplemented(
            "non-primitive value not supported yet".to_string(),
        )),
    }
}

pub fn process_expr<W: Write>(
    expr_id: MetaExprId,
    ctx: &mut MetaProcessContext<W>,
) -> Result<RuntimeExprId, MetaProcessError> {
    match ctx
        .meta_ast
        .get_expr(expr_id)
        .ok_or(MetaProcessError::ExprNotFound(expr_id))?
    {
        MetaExpr::Int(i) => {
            let expr = RuntimeExpr::Int(*i);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }
        MetaExpr::String(s) => {
            let expr = RuntimeExpr::String(s.clone());
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }
        MetaExpr::Bool(b) => {
            let expr = RuntimeExpr::Bool(*b);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::StructLiteral { type_name, fields } => {
            let mut out_fields = Vec::new();
            for (name, expr) in fields {
                let expanded = process_expr(*expr, ctx)?;
                out_fields.push((name.clone(), expanded));
            }

            let expr = RuntimeExpr::StructLiteral {
                type_name: type_name.clone(),
                fields: out_fields,
            };
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        //MetaExpr::Variable(name) => match ctx.env.borrow().get(name) {
        //    Some(x) => value_to_literal(x, ctx),
        //    None => Ok(RuntimeExpr::Variable(name.clone())),
        //},
        //TODO replace the value to lit inlining
        MetaExpr::Variable(name) => {
            let expr = RuntimeExpr::Variable(name.clone());
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::List(exprs) => {
            let expr = RuntimeExpr::List(process_exprs(exprs, ctx)?);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::Add(a, b) => {
            let expr = RuntimeExpr::Add(process_expr(*a, ctx)?, process_expr(*b, ctx)?);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::Sub(a, b) => {
            let expr = RuntimeExpr::Sub(process_expr(*a, ctx)?, process_expr(*b, ctx)?);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::Mult(a, b) => {
            let expr = RuntimeExpr::Mult(process_expr(*a, ctx)?, process_expr(*b, ctx)?);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::Div(a, b) => {
            let expr = RuntimeExpr::Div(process_expr(*a, ctx)?, process_expr(*b, ctx)?);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::Equals(a, b) => {
            let expr = RuntimeExpr::Equals(process_expr(*a, ctx)?, process_expr(*b, ctx)?);
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }

        MetaExpr::Call { callee, args } => {
            let mut out_args = Vec::new();
            for e in args {
                out_args.push(process_expr(*e, ctx)?);
            }

            let call_expr = RuntimeExpr::Call {
                callee: callee.clone(),
                args: out_args,
            };

            let id = ctx.runtime_ast.insert_expr(call_expr);
            Ok(id)

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
        }

        MetaExpr::Typeof(ident) => {
            //let def = ctx
            //    .decls
            //    .get_struct(ident)
            //    .ok_or_else(|| MetaProcessError::UnknownType(ident.clone()))?;

            let type_expr = RuntimeExpr::String(ident.clone());
            let id = ctx.runtime_ast.insert_expr(type_expr);
            Ok(id)
        }

        MetaExpr::Embed(file_path) => {
            let expr = RuntimeExpr::String(file_path.clone());
            let id = ctx.runtime_ast.insert_expr(expr);
            Ok(id)
        }
    }
}

pub fn process_exprs<W: Write>(
    exprs: &Vec<MetaExprId>,
    ctx: &mut MetaProcessContext<W>,
) -> Result<Vec<RuntimeExprId>, MetaProcessError> {
    let mut output = Vec::new();

    for expr in exprs {
        output.push(process_expr(*expr, ctx)?);
    }

    Ok(output)
}

pub fn process_stmt<W: Write>(
    stmt_id: MetaStmtId,
    ctx: &mut MetaProcessContext<W>,
) -> Result<Vec<RuntimeStmtId>, MetaProcessError> {
    let ast_stmt = ctx
        .meta_ast
        .get_stmt(stmt_id)
        .ok_or(MetaProcessError::StmtNotFound(stmt_id))?;
    match ast_stmt {
        MetaStmt::ExprStmt(expr_id) => {
            let runtime_expr_id = process_expr(*expr_id, ctx)?;
            let stmt = RuntimeStmt::ExprStmt(runtime_expr_id);
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::VarDecl { name, expr } => {
            let expr = process_expr(*expr, ctx)?;
            let stmt = RuntimeStmt::VarDecl {
                name: name.clone(),
                expr,
            };
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::Print(expr) => {
            let expr = process_expr(*expr, ctx)?;
            let stmt = RuntimeStmt::Print(expr);
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::If {
            cond,
            body,
            else_branch,
        } => {
            let else_branch = match else_branch {
                Some(stmt) => {
                    let stmt = process_stmt(*stmt, ctx)?;
                    let block = RuntimeStmt::Block(stmt);
                    let block_id = ctx.runtime_ast.insert_stmt(block);
                    Some(block_id)
                }
                None => None,
            };
            let cond = process_expr(*cond, ctx)?;
            let body = process_to_block(*body, ctx)?;
            let if_stmt = RuntimeStmt::If {
                cond,
                body,
                else_branch,
            };
            let id = ctx.runtime_ast.insert_stmt(if_stmt);
            Ok(vec![id])
        }

        MetaStmt::ForEach {
            var,
            iterable,
            body,
        } => {
            let iterable_id = process_expr(*iterable, ctx)?;
            let body = process_to_block(*body, ctx)?;
            let stmt = RuntimeStmt::ForEach {
                var: var.clone(),
                iterable: iterable_id,
                body,
            };
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::Block(stmts) => {
            let mut processed = Vec::new();
            for stmt in stmts {
                processed.extend(process_stmt(*stmt, ctx)?);
            }
            let stmt = RuntimeStmt::Block(processed);
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::FnDecl { name, params, body } => {
            let processed_body = process_to_block(*body, ctx)?;

            let stmt = RuntimeStmt::FnDecl {
                name: name.clone(),
                params: params.clone(),
                body: processed_body,
            };
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::StructDecl { name, fields } => {
            //ctx.decls.define_struct(
            //    name.clone(),
            //    StructDef {
            //        fields: fields.clone(),
            //    },
            //);
            Ok(vec![])
        }

        MetaStmt::Return(expr) => {
            let expr = match expr {
                Some(e) => Some(process_expr(*e, ctx)?),
                None => None,
            };
            let stmt = RuntimeStmt::Return(expr);
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::Gen(stmts) => {
            let stmt = RuntimeStmt::Gen(process_stmts(stmts, ctx)?);
            let id = ctx.runtime_ast.insert_stmt(stmt);
            Ok(vec![id])
        }

        MetaStmt::MetaBlock(parsed_stmt) => {
            let processed_code = process_stmt(*parsed_stmt, ctx)?;

            eval(
                ctx.runtime_ast,
                &processed_code,
                Environment::new(),
                &mut None,
                ctx.out,
            )?;

            Ok(vec![])
        }

        MetaStmt::Import(_mod_name) => Ok(vec![]),
    }
}

// TODO this method is a bit awkward because of the plurality of processing stmts. This can be
// refactored later
fn process_to_block<W: Write>(
    stmt_id: MetaStmtId,
    ctx: &mut MetaProcessContext<W>,
) -> Result<RuntimeStmtId, MetaProcessError> {
    match ctx
        .meta_ast
        .get_stmt(stmt_id)
        .ok_or(MetaProcessError::StmtNotFound(stmt_id))?
    {
        MetaStmt::Block(_) => {
            let processed = process_stmt(stmt_id, ctx)?;
            debug_assert!(
                processed.len() == 1,
                "block processing must produce exactly one statement"
            );
            Ok(processed.into_iter().next().unwrap())
        }

        _ => {
            let block = RuntimeStmt::Block(process_stmt(stmt_id, ctx)?);
            let id = ctx.runtime_ast.insert_stmt(block);
            Ok(id)
        }
    }
}

pub fn process_stmts<W: Write>(
    stmts: &Vec<MetaStmtId>,
    ctx: &mut MetaProcessContext<W>,
) -> Result<Vec<RuntimeStmtId>, MetaProcessError> {
    let mut output = Vec::new();

    for stmt in stmts {
        let outputs = process_stmt(*stmt, ctx)?;
        output.extend(outputs);
    }

    Ok(output)
}

pub fn process<W: Write>(ast: &MetaAst, out: &mut W) -> Result<RuntimeAst, MetaProcessError> {
    let mut runtime_ast = RuntimeAst::new();
    let mut ctx = MetaProcessContext {
        meta_ast: ast,
        runtime_ast: &mut runtime_ast,
        out,
    };

    runtime_ast.sem_root_stmts = process_stmts(&ctx.meta_ast.sem_root_stmts, &mut ctx)?;
    Ok(runtime_ast)
}
