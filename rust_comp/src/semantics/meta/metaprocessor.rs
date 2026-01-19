use crate::frontend::blueprint_ast::{BlueprintAst, BlueprintExpr, BlueprintStmt};
use crate::runtime::environment::EnvRef;
use crate::runtime::value::{Function, Value};
use crate::semantics::meta::expanded_ast::{ExpandedExpr, ExpandedStmt};
use crate::util::external_resolver::ExternalResolver;
use crate::{
    runtime::interpreter::{self, EvalError},
    util::decl_registry::{DeclRegistry, StructDef},
};
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

pub struct MetaProcessContext<'a, E: ExternalResolver, W: Write> {
    pub env: EnvRef,
    pub decls: &'a mut DeclRegistry,
    pub resolver: &'a mut E,
    pub out: &'a mut W,
    pub curr_dir: &'a Path,
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
    pub emitted: Vec<ExpandedStmt>,
}

pub fn value_to_literal(val: Value) -> Result<ExpandedExpr, MetaProcessError> {
    match val {
        Value::Int(n) => Ok(ExpandedExpr::Int(n)),
        Value::String(s) => Ok(ExpandedExpr::String(s)),
        Value::Bool(b) => Ok(ExpandedExpr::Bool(b)),
        Value::Unit => Err(MetaProcessError::Unimplemented(
            "Unit has no literal representation".to_string(),
        )),
        _ => Err(MetaProcessError::Unimplemented(
            "non-primitive value not supported yet".to_string(),
        )),
    }
}

pub fn process_expr<E: ExternalResolver, W: Write>(
    expr: &BlueprintExpr,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<ExpandedExpr, MetaProcessError> {
    match expr {
        BlueprintExpr::Int(i) => Ok(ExpandedExpr::Int(*i)),
        BlueprintExpr::String(s) => Ok(ExpandedExpr::String(s.clone())),
        BlueprintExpr::Bool(b) => Ok(ExpandedExpr::Bool(*b)),

        BlueprintExpr::StructLiteral { type_name, fields } => {
            let mut out_fields = Vec::new();
            for (name, expr) in fields {
                let expanded = process_expr(expr, ctx)?;
                out_fields.push((name.clone(), Box::new(expanded)));
            }

            Ok(ExpandedExpr::StructLiteral {
                type_name: type_name.clone(),
                fields: out_fields,
            })
        }

        BlueprintExpr::Variable(name) => match ctx.env.borrow().get(name) {
            Some(x) => value_to_literal(x),
            None => Ok(ExpandedExpr::Variable(name.clone())),
        },

        BlueprintExpr::List(exprs) => Ok(ExpandedExpr::List(process_exprs(exprs, ctx)?)),

        BlueprintExpr::Add(a, b) => Ok(ExpandedExpr::Add(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        BlueprintExpr::Sub(a, b) => Ok(ExpandedExpr::Sub(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        BlueprintExpr::Mult(a, b) => Ok(ExpandedExpr::Mult(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        BlueprintExpr::Div(a, b) => Ok(ExpandedExpr::Div(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        BlueprintExpr::Equals(a, b) => Ok(ExpandedExpr::Equals(
            Box::new(process_expr(a, ctx)?),
            Box::new(process_expr(b, ctx)?),
        )),

        BlueprintExpr::Call { callee, args } => {
            let mut out_args = Vec::new();
            for e in args {
                out_args.push(process_expr(e, ctx)?);
            }

            let call_expr = ExpandedExpr::Call {
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

        BlueprintExpr::Typeof(id) => {
            let def = ctx
                .decls
                .get_struct(id)
                .ok_or_else(|| MetaProcessError::UnknownType(id.clone()))?;

            Ok(ExpandedExpr::String(def.to_string()))
        }

        BlueprintExpr::Embed(file_path) => {
            let contents = ctx
                .resolver
                .read_file(ctx.curr_dir, file_path)
                .ok_or_else(|| MetaProcessError::EmbedFailed {
                    path: file_path.clone(),
                    error: "file not found".to_string(),
                })?;
            Ok(ExpandedExpr::String(contents))
        }
    }
}

pub fn process_exprs<E: ExternalResolver, W: Write>(
    exprs: &Vec<BlueprintExpr>,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<Vec<ExpandedExpr>, MetaProcessError> {
    let mut output = Vec::new();

    for expr in exprs {
        output.push(process_expr(expr, ctx)?);
    }

    Ok(output)
}

pub fn process_stmt<E: ExternalResolver, W: Write>(
    stmt: &BlueprintStmt,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<Vec<ExpandedStmt>, MetaProcessError> {
    match stmt {
        BlueprintStmt::ExprStmt(expr) => Ok(vec![ExpandedStmt::ExprStmt(Box::new(process_expr(
            expr, ctx,
        )?))]),

        BlueprintStmt::Assignment { name, expr } => Ok(vec![ExpandedStmt::Assignment {
            name: name.clone(),
            expr: Box::new(process_expr(expr, ctx)?),
        }]),

        BlueprintStmt::Print(expr) => Ok(vec![ExpandedStmt::Print(Box::new(process_expr(
            expr, ctx,
        )?))]),

        BlueprintStmt::If {
            cond,
            body,
            else_branch,
        } => {
            let else_branch = match else_branch {
                Some(stmt) => Some(Box::new(ExpandedStmt::Block(process_stmt(stmt, ctx)?))),
                None => None,
            };
            Ok(vec![ExpandedStmt::If {
                cond: Box::new(process_expr(cond, ctx)?),
                body: Box::new(process_to_block(body, ctx)?),
                else_branch,
            }])
        }

        BlueprintStmt::ForEach {
            var,
            iterable,
            body,
        } => Ok(vec![ExpandedStmt::ForEach {
            var: var.clone(),
            iterable: Box::new(process_expr(iterable, ctx)?),
            body: Box::new(process_to_block(body, ctx)?),
        }]),

        BlueprintStmt::Block(stmts) => {
            let mut processed = Vec::new();
            for stmt in stmts {
                processed.extend(process_stmt(stmt, ctx)?);
            }
            Ok(vec![ExpandedStmt::Block(processed)])
        }

        BlueprintStmt::FnDecl {
            name,
            func_type,
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
                let func_decl = ExpandedStmt::FnDecl {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(processed_body.clone()),
                };
                return Ok(vec![func_decl]);
            }
            Ok(vec![])
        }

        BlueprintStmt::StructDecl { name, fields } => {
            ctx.decls.define_struct(
                name.clone(),
                StructDef {
                    fields: fields.clone(),
                },
            );
            Ok(vec![])
        }

        BlueprintStmt::Return(expr) => {
            let expr = match expr {
                Some(e) => Some(Box::new(process_expr(e, ctx)?)),
                None => None,
            };
            Ok(vec![ExpandedStmt::Return(expr)])
        }

        BlueprintStmt::Gen(stmts) => Ok(vec![ExpandedStmt::Gen(process_stmts(stmts, ctx)?)]),

        BlueprintStmt::MetaStmt(parsed_stmt) => {
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

        BlueprintStmt::Import(_mod_name) => Ok(vec![]),
    }
}

fn process_to_block<E: ExternalResolver, W: Write>(
    stmt: &BlueprintStmt,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<ExpandedStmt, MetaProcessError> {
    match stmt {
        BlueprintStmt::Block(_) => {
            let processed = process_stmt(stmt, ctx)?;
            debug_assert!(
                processed.len() == 1,
                "block processing must produce exactly one statement"
            );
            Ok(processed.into_iter().next().unwrap())
        }

        _ => Ok(ExpandedStmt::Block(process_stmt(stmt, ctx)?)),
    }
}

pub fn process_stmts<E: ExternalResolver, W: Write>(
    stmts: &Vec<BlueprintStmt>,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<Vec<ExpandedStmt>, MetaProcessError> {
    let mut output = Vec::new();

    for stmt in stmts {
        output.extend(process_stmt(stmt, ctx)?);
    }

    Ok(output)
}

pub fn process<E: ExternalResolver, W: Write>(
    ast: &BlueprintAst,
    ctx: &mut MetaProcessContext<E, W>,
) -> Result<Vec<ExpandedStmt>, MetaProcessError> {
    process_stmts(&ast.stmts, ctx)
}
