use crate::frontend::meta_ast::*;
use crate::semantics::meta::runtime_ast::*;
use crate::runtime::value::{Function, Value};
use crate::{
    runtime::interpreter::{self, EvalError},
    util::decl_registry::{StructDef},
};
use std::io::Write;

pub struct MetaProcessContext<'a, W: Write> {
    pub out: &'a mut W,
}

#[derive(Debug)]
pub enum MetaProcessError {
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

pub fn value_to_literal(val: Value) -> Result<RuntimeExprId, MetaProcessError> {
    match val {
        Value::Int(n) => Ok(RuntimeExpr::Int(n)),
        Value::String(s) => Ok(RuntimeExpr::String(s)),
        Value::Bool(b) => Ok(RuntimeExpr::Bool(b)),
        Value::Unit => Err(MetaProcessError::Unimplemented(
            "Unit has no literal representation".to_string(),
        )),
        _ => Err(MetaProcessError::Unimplemented(
            "non-primitive value not supported yet".to_string(),
        )),
    }
}

pub fn process_expr<W: Write>(
    expr: &MetaExpr,
    ctx: &mut MetaProcessContext<W>,
) -> Result<RuntimeExprId, MetaProcessError> {
    match expr {
        MetaExpr::Int(i) => Ok(RuntimeExpr::Int(*i)),
        MetaExpr::String(s) => Ok(RuntimeExpr::String(s.clone())),
        MetaExpr::Bool(b) => Ok(RuntimeExpr::Bool(*b)),

        MetaExpr::StructLiteral { type_name, fields } => {
            let mut out_fields = Vec::new();
            for (name, expr) in fields {
                let expanded = process_expr(expr, ctx)?;
                out_fields.push((name.clone(), Box::new(expanded)));
            }

            Ok(RuntimeExpr::StructLiteral {
                type_name: type_name.clone(),
                fields: out_fields,
            })
        }

        MetaExpr::Variable(name) => match ctx.env.borrow().get(name) {
            Some(x) => value_to_literal(x),
            None => Ok(RuntimeExpr::Variable(name.clone())),
        },

        MetaExpr::List(exprs) => Ok(RuntimeExpr::List(process_exprs(exprs, ctx)?)),

        MetaExpr::Add(a, b) => Ok(RuntimeExpr::Add(
            process_expr(a, ctx)?,
            process_expr(b, ctx)?,
        )),

        MetaExpr::Sub(a, b) => Ok(RuntimeExpr::Sub(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        MetaExpr::Mult(a, b) => Ok(RuntimeExpr::Mult(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        MetaExpr::Div(a, b) => Ok(RuntimeExpr::Div(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        MetaExpr::Equals(a, b) => Ok(RuntimeExpr::Equals(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        MetaExpr::Call { callee, args } => {
            let mut out_args = Vec::new();
            for e in args {
                out_args.push(process_expr(e, ctx)?);
            }

            let call_expr = RuntimeExpr::Call {
                callee: callee.clone(),
                args: out_args,
            };

            match ctx.env.borrow().get(&callee) {
                Some(_) => {
                    let val = interpreter::eval_expr(
                        &call_expr,
                        ctx.env.clone(),
                        ctx.decls,
                        &mut None,
                        ctx.out,
                    )?;
                    value_to_literal(val)
                }
                None => Ok(call_expr),
            }
        }
        
        MetaExpr::Typeof(id) => {
            let def = ctx
                .decls
                .get_struct(id)
                .ok_or_else(|| MetaProcessError::UnknownType(id.clone()))?;

            Ok(RuntimeExpr::String(def.to_string()))
        }

        MetaExpr::Embed(file_path) => {
            let contents = ctx.resolver
                .read_file(ctx.curr_dir, file_path)
                .ok_or_else(|| MetaProcessError::EmbedFailed {
                    path: file_path.clone(),
                    error: "file not found".to_string(),
                })?;
            Ok(RuntimeExpr::String(contents))
        }

    }
}

pub fn process_exprs<W: Write>(
    exprs: &Vec<MetaExpr>,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<Vec<RuntimeExpr>, MetaProcessError> {
    let mut output = Vec::new();

    for expr in exprs {
        output.push(process_expr(expr, ctx)?);
    }

    Ok(output)
}

pub fn process_stmt<W: Write>(
    stmt: &MetaStmt,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<Vec<RuntimeStmtId>, MetaProcessError> {
    match stmt {
        MetaStmt::ExprStmt(expr) => Ok(vec![RuntimeStmt::ExprStmt(Box::new(process_expr(
            expr, ctx,
        )?))]),

        MetaStmt::Assignment { name, expr } => Ok(vec![RuntimeStmt::Assignment {
            name: name.clone(),
            expr: Box::new(process_expr(expr, ctx)?),
        }]),

        MetaStmt::Print(expr) => Ok(vec![RuntimeStmt::Print(Box::new(process_expr(
            expr, ctx,
        )?))]),

        MetaStmt::If {
            cond,
            body,
            else_branch,
        } => {
            let else_branch = match else_branch {
                Some(stmt) => Some(Box::new(RuntimeStmt::Block(process_stmt(stmt, ctx)?))),
                None => None,
            };
            Ok(vec![RuntimeStmt::If {
                cond: Box::new(process_expr(cond, ctx)?),
                body: Box::new(process_to_block(body, ctx)?),
                else_branch,
            }])
        }

        MetaStmt::ForEach {
            var,
            iterable,
            body,
        } => Ok(vec![RuntimeStmt::ForEach {
            var: var.clone(),
            iterable: Box::new(process_expr(iterable, ctx)?),
            body: Box::new(process_to_block(body, ctx)?),
        }]),

        MetaStmt::Block(stmts) => {
            let mut processed = Vec::new();
            for stmt in stmts {
                processed.extend(process_stmt(stmt, ctx)?);
            }
            Ok(vec![RuntimeStmt::Block(processed)])
        }

        MetaStmt::FnDecl {
            name,
            params,
            body,
        } => {
            let processed_body = process_to_block(body.as_ref(), ctx)?;
            if func_type.can_execute_at_meta() {
                let func = Rc::new(Function {
                    params: params.clone(),
                    body: Box::new(processed_body.clone()),
                    env: Rc::clone(&ctx.env),
                });
                ctx.env
                    .borrow_mut()
                    .define(name.clone(), Value::Function(func));
            }

            if func_type.can_execute_at_runtime() {
                let func_decl = RuntimeStmt::FnDecl {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(processed_body.clone()),
                };
                return Ok(vec![func_decl]);
            }
            Ok(vec![])
        }

        MetaStmt::StructDecl { name, fields } => {
            ctx.decls.define_struct(
                name.clone(),
                StructDef {
                    fields: fields.clone(),
                },
            );
            Ok(vec![])
        }

        MetaStmt::Return(expr) => {
            let expr = match expr {
                Some(e) => Some(Box::new(process_expr(e, ctx)?)),
                None => None,
            };
            Ok(vec![RuntimeStmt::Return(expr)])
        }

        MetaStmt::Gen(stmts) => Ok(vec![RuntimeStmt::Gen(process_stmts(stmts, ctx)?)]),

        MetaStmt::MetaStmt(parsed_stmt) => {
            let processed_code = process_stmt(parsed_stmt, ctx)?;
            let mut meta_ctx = MetaContext {
                emitted: Vec::new(),
            };

            interpreter::eval(
                &processed_code,
                ctx.env.clone(),
                ctx.decls,
                &mut Some(&mut meta_ctx),
                ctx.out,
            )?;

            Ok(meta_ctx.emitted)
        }

        MetaStmt::Import(_mod_name) => {
            Ok(vec![])
        }
    }
}

fn process_to_block<W: Write>(
    stmt: &MetaStmt,
    ctx: &mut MetaProcessContext<W>,
) -> Result<RuntimeStmtId, MetaProcessError> {
    match stmt {
        MetaStmt::Block(_) => {
            let processed = process_stmt(stmt, ctx)?;
            debug_assert!(
                processed.len() == 1,
                "block processing must produce exactly one statement"
            );
            Ok(processed.into_iter().next().unwrap())
        }

        _ => Ok(RuntimeStmt::Block(process_stmt(stmt, ctx)?)),
    }
}

pub fn process_stmts<W: Write>(
    stmts: &Vec<MetaStmtId>,
    ctx: &mut MetaProcessContext<W>,
) -> Result<Vec<RuntimeStmtId>, MetaProcessError> {
    let mut output = Vec::new();

    for stmt in stmts {
        output.extend(process_stmt(stmt, ctx)?);
    }

    Ok(output)
}

pub fn process<W: Write>(
    ast: &MetaAst,
    ctx: &mut MetaProcessContext<W>,
) -> Result<Vec<RuntimeStmtId>, MetaProcessError> {
    process_stmts(&ast.sem_root_stmts, ctx)
}
