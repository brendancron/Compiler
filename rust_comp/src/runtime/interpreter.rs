use crate::semantics::meta::metaprocessor::MetaContext;
use crate::semantics::meta::substitution::subst_stmts;
use crate::util::decl_registry::DeclRegistry;
use super::environment::{Env, EnvRef};
use super::result::ExecResult;
use crate::semantics::meta::expanded_ast::{ExpandedExpr, ExpandedStmt};
use crate::semantics::types::types::{self, Type};
use super::value::{Function, Value};
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

#[derive(Debug)]
pub enum EvalError {
    UnknownStructType(String),
    UndefinedVariable(String),
    TypeError(Type),
    NonFunctionCall,
    ArgumentMismatch,
}

pub struct EvalCtx<W> {
    pub out: W,
}

pub fn eval_expr<W: Write>(
    expr: &ExpandedExpr,
    env: EnvRef,
    decls: &mut DeclRegistry,
    ctx: &mut Option<&mut MetaContext>,
    out: &mut W,
) -> Result<Value, EvalError> {
    match expr {
        ExpandedExpr::Int(n) => Ok(Value::Int(*n)),
        ExpandedExpr::String(s) => Ok(Value::String(s.clone())),
        ExpandedExpr::Bool(b) => Ok(Value::Bool(*b)),

        ExpandedExpr::StructLiteral { type_name, fields } => {
            let _struct_def = decls
                .get_struct(type_name)
                .ok_or_else(|| EvalError::UnknownStructType(type_name.clone()))?;

            let mut fs = vec![];

            for (field_name, expr) in fields {
                let value = eval_expr(expr, env.clone(), decls, ctx, out)?;
                fs.push((field_name.clone(), value));
            }

            Ok(Value::Struct {
                type_name: type_name.clone(),
                fields: Rc::new(RefCell::new(fs)),
            })
        }

        ExpandedExpr::Variable(name) => {
            let var = env
                .borrow()
                .get(name)
                .ok_or_else(|| EvalError::UndefinedVariable(name.clone()))?;
            Ok(var)
        }

        ExpandedExpr::List(exprs) => {
            let mut values = Vec::new();
            for e in exprs {
                values.push(eval_expr(e, env.clone(), decls, ctx, out)?);
            }

            Ok(Value::List(Rc::new(RefCell::new(values))))
        }

        ExpandedExpr::Add(a, b) => match (
            eval_expr(a, env.clone(), decls, ctx, out)?,
            eval_expr(b, env.clone(), decls, ctx, out)?,
        ) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
            _ => Err(EvalError::TypeError(types::int_type())),
        },

        ExpandedExpr::Sub(a, b) => match (
            eval_expr(a, env.clone(), decls, ctx, out)?,
            eval_expr(b, env.clone(), decls, ctx, out)?,
        ) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
            _ => Err(EvalError::TypeError(types::int_type())),
        },

        ExpandedExpr::Mult(a, b) => match (
            eval_expr(a, env.clone(), decls, ctx, out)?,
            eval_expr(b, env.clone(), decls, ctx, out)?,
        ) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x * y)),
            _ => Err(EvalError::TypeError(types::int_type())),
        },

        ExpandedExpr::Div(a, b) => match (
            eval_expr(a, env.clone(), decls, ctx, out)?,
            eval_expr(b, env.clone(), decls, ctx, out)?,
        ) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x / y)),
            _ => Err(EvalError::TypeError(types::int_type())),
        },

        ExpandedExpr::Equals(a, b) => match (
            eval_expr(a, env.clone(), decls, ctx, out)?,
            eval_expr(b, env.clone(), decls, ctx, out)?,
        ) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x == y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Bool(x == y)),
            (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x == y)),
            _ => Err(EvalError::TypeError(types::unit_type())),
        },

        ExpandedExpr::Call { callee, args } => {
            let func = match env.borrow().get(callee) {
                Some(Value::Function(f)) => f,
                _ => return Err(EvalError::NonFunctionCall),
            };

            if func.params.len() != args.len() {
                return Err(EvalError::ArgumentMismatch);
            }

            let arg_vals = args.iter().try_fold(Vec::new(), |mut v, a| {
                v.push(eval_expr(a, env.clone(), decls, ctx, out)?);
                Ok(v)
            })?;

            let callee_env = Env::new_child(Rc::clone(&func.env));

            {
                let mut e = callee_env.borrow_mut();
                for (param, value) in func.params.iter().zip(arg_vals) {
                    e.define(param.clone(), value);
                }
            }

            let result = match eval_stmt(&func.body, callee_env, decls, ctx, out)? {
                ExecResult::Return(v) => v,
                ExecResult::Continue => Value::Unit,
            };

            Ok(result)
        }
    }
}

pub fn eval_stmt<W: Write>(
    stmt: &ExpandedStmt,
    env: EnvRef,
    decls: &mut DeclRegistry,
    ctx: &mut Option<&mut MetaContext>,
    out: &mut W,
) -> Result<ExecResult, EvalError> {
    match stmt {
        ExpandedStmt::Print(expr) => {
            let value = eval_expr(expr, env.clone(), decls, ctx, out)?;
            writeln!(out, "{}", value).unwrap();
            Ok(ExecResult::Continue)
        }

        ExpandedStmt::If {
            cond,
            body,
            else_branch,
        } => match eval_expr(cond, env.clone(), decls, ctx, out)? {
            Value::Bool(true) => eval_stmt(body, env.clone(), decls, ctx, out),
            Value::Bool(false) => match else_branch {
                Some(else_stmt) => eval_stmt(else_stmt, env.clone(), decls, ctx, out),
                None => Ok(ExecResult::Continue),
            },
            _ => Err(EvalError::TypeError(types::bool_type())),
        },

        ExpandedStmt::ForEach {
            var,
            iterable,
            body,
        } => {
            let value = eval_expr(iterable, env.clone(), decls, ctx, out);

            for elem in value?.enumerate().iter() {
                env.borrow_mut().push_scope();
                env.borrow_mut().define(var.clone(), elem.clone());

                match eval_stmt(body, env.clone(), decls, ctx, out)? {
                    ExecResult::Return(v) => {
                        env.borrow_mut().pop_scope();
                        return Ok(ExecResult::Return(v));
                    }
                    ExecResult::Continue => {}
                }

                env.borrow_mut().pop_scope();
            }

            Ok(ExecResult::Continue)
        }

        ExpandedStmt::ExprStmt(expr) => {
            eval_expr(expr, env.clone(), decls, ctx, out)?;
            Ok(ExecResult::Continue)
        }

        ExpandedStmt::Assignment { name, expr } => {
            let value = eval_expr(expr, env.clone(), decls, ctx, out)?;
            env.borrow_mut().define(name.clone(), value);
            Ok(ExecResult::Continue)
        }

        ExpandedStmt::Block(stmts) => {
            env.borrow_mut().push_scope();
            let res = eval(stmts, env.clone(), decls, ctx, out);
            env.borrow_mut().pop_scope();
            res
        }

        ExpandedStmt::FnDecl { name, params, body } => {
            let func = Rc::new(Function {
                params: params.clone(),
                body: body.clone(),
                env: Rc::clone(&env),
            });

            env.borrow_mut().define(name.clone(), Value::Function(func));

            Ok(ExecResult::Continue)
        }

        ExpandedStmt::Return(opt_expr) => {
            let val = match opt_expr {
                None => Value::Unit,
                Some(expr) => eval_expr(expr, env, decls, ctx, out)?,
            };
            Ok(ExecResult::Return(val))
        }

        ExpandedStmt::Gen(stmts) => {
            let meta = ctx.as_deref_mut().expect("gen outside meta");
            let substituted = subst_stmts(stmts, &env);
            for stmt in substituted {
                meta.emitted.push(stmt.clone());
            }
            Ok(ExecResult::Continue)
        }
    }
}

pub fn eval<W: Write>(
    stmts: &Vec<ExpandedStmt>,
    env: EnvRef,
    decls: &mut DeclRegistry,
    ctx: &mut Option<&mut MetaContext>,
    out: &mut W,
) -> Result<ExecResult, EvalError> {
    for stmt in stmts {
        match eval_stmt(stmt, env.clone(), decls, ctx, out)? {
            ExecResult::Continue => {}
            ExecResult::Return(v) => {
                return Ok(ExecResult::Return(v));
            }
        }
    }
    Ok(ExecResult::Continue)
}
