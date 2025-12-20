use crate::components::metaprocessor::MetaContext;
use crate::components::substitution::subst_stmts;
use crate::models::ast::{LoweredExpr, LoweredStmt};
use crate::models::environment::{Env, EnvRef};
use crate::models::result::ExecResult;
use crate::models::value::{Function, Value};
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval_expr(expr: &LoweredExpr, env: EnvRef, ctx: &mut Option<&mut MetaContext>) -> Value {
    match expr {
        LoweredExpr::Int(n) => Value::Int(*n),
        LoweredExpr::String(s) => Value::String(s.clone()),
        LoweredExpr::Bool(b) => Value::Bool(*b),
        LoweredExpr::Variable(name) => env
            .borrow()
            .get(name)
            .unwrap_or_else(|| panic!("undefined variable: {}", name)),

        LoweredExpr::List(exprs) => {
            let values = exprs
                .iter()
                .map(|e| eval_expr(e, env.clone(), ctx))
                .collect();

            Value::List(Rc::new(RefCell::new(values)))
        }

        LoweredExpr::Add(a, b) => match (
            eval_expr(a, env.clone(), ctx),
            eval_expr(b, env.clone(), ctx),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
            _ => panic!("type error: + expects ints"),
        },

        LoweredExpr::Sub(a, b) => match (
            eval_expr(a, env.clone(), ctx),
            eval_expr(b, env.clone(), ctx),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
            _ => panic!("type error: - expects ints"),
        },

        LoweredExpr::Mult(a, b) => match (
            eval_expr(a, env.clone(), ctx),
            eval_expr(b, env.clone(), ctx),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
            _ => panic!("type error: * expects ints"),
        },

        LoweredExpr::Div(a, b) => match (
            eval_expr(a, env.clone(), ctx),
            eval_expr(b, env.clone(), ctx),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
            _ => panic!("type error: / expects ints"),
        },

        LoweredExpr::Equals(a, b) => match (
            eval_expr(a, env.clone(), ctx),
            eval_expr(b, env.clone(), ctx),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Bool(x == y),
            (Value::String(x), Value::String(y)) => Value::Bool(x == y),
            (Value::Bool(x), Value::Bool(y)) => Value::Bool(x == y),
            _ => panic!("type error: == mismatched types"),
        },

        LoweredExpr::Call { callee, args } => {
            let callee_val = eval_expr(callee, env.clone(), ctx);
            let func = match callee_val {
                Value::Function(f) => f,
                _ => panic!("attempted to call a non-function"),
            };

            if func.params.len() != args.len() {
                panic!("wrong number of arguments");
            }

            let arg_vals: Vec<Value> = args
                .iter()
                .map(|a| eval_expr(a, env.clone(), ctx))
                .collect();

            let callee_env = Env::new_child(Rc::clone(&func.env));

            {
                let mut e = callee_env.borrow_mut();
                for (param, value) in func.params.iter().zip(arg_vals) {
                    e.define(param.clone(), value);
                }
            }

            let result = match eval_stmt(&func.body, callee_env, ctx) {
                ExecResult::Return(v) => v,
                ExecResult::Continue => Value::Unit,
            };

            // We can skip this since we don't use it again
            // callee_env.pop_scope();

            result
        }
    }
}

pub fn eval_stmt(
    stmt: &LoweredStmt,
    env: EnvRef,
    ctx: &mut Option<&mut MetaContext>,
) -> ExecResult {
    match stmt {
        LoweredStmt::Print(expr) => {
            let value = eval_expr(expr, env.clone(), ctx);
            println!("{}", value);
            ExecResult::Continue
        }

        LoweredStmt::If {
            cond,
            body,
            else_branch,
        } => match eval_expr(cond, env.clone(), ctx) {
            Value::Bool(true) => eval_stmt(body, env.clone(), ctx),
            Value::Bool(false) => match else_branch {
                Some(else_stmt) => eval_stmt(else_stmt, env.clone(), ctx),
                None => ExecResult::Continue,
            },
            _ => panic!("type error: expected bool expr"),
        },

        LoweredStmt::ForEach {
            var,
            iterable,
            body,
        } => {
            let value = eval_expr(iterable, env.clone(), ctx);

            for elem in value.enumerate().iter() {
                env.borrow_mut().push_scope();
                env.borrow_mut().define(var.clone(), elem.clone());

                match eval_stmt(body, env.clone(), ctx) {
                    ExecResult::Return(v) => {
                        env.borrow_mut().pop_scope();
                        return ExecResult::Return(v);
                    }
                    ExecResult::Continue => {}
                }

                env.borrow_mut().pop_scope();
            }

            ExecResult::Continue
        }

        LoweredStmt::ExprStmt(expr) => {
            eval_expr(expr, env.clone(), ctx);
            ExecResult::Continue
        }

        LoweredStmt::Assignment { name, expr } => {
            let value = eval_expr(expr, env.clone(), ctx);
            env.borrow_mut().define(name.clone(), value);
            ExecResult::Continue
        }

        LoweredStmt::Block(stmts) => {
            env.borrow_mut().push_scope();
            let res = eval(stmts, env.clone(), ctx);
            env.borrow_mut().pop_scope();
            res
        }

        LoweredStmt::FnDecl { name, params, body } => {
            env.borrow_mut().define(name.clone(), Value::Unit);

            let func = Rc::new(Function {
                params: params.clone(),
                body: *body.clone(),
                env: Rc::clone(&env),
            });

            env.borrow_mut().assign(name.clone(), Value::Function(func));

            ExecResult::Continue
        }

        LoweredStmt::Return(opt_expr) => {
            let val = match opt_expr {
                None => Value::Unit,
                Some(expr) => eval_expr(expr, env, ctx),
            };
            ExecResult::Return(val)
        }

        LoweredStmt::Gen(stmts) => {
            let meta = ctx.as_deref_mut().expect("gen outside meta");
            let substituted = subst_stmts(stmts, &env);
            for stmt in substituted {
                meta.emitted.push(stmt.clone());
            }
            ExecResult::Continue
        }
    }
}

pub fn eval(
    stmts: &Vec<LoweredStmt>,
    env: EnvRef,
    ctx: &mut Option<&mut MetaContext>,
) -> ExecResult {
    for stmt in stmts {
        match eval_stmt(stmt, env.clone(), ctx) {
            ExecResult::Continue => {}
            ExecResult::Return(v) => {
                return ExecResult::Return(v);
            }
        }
    }
    ExecResult::Continue
}
