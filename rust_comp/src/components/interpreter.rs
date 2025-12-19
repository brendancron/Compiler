use crate::components::metaprocessor::MetaContext;
use crate::models::ast::{LoweredExpr, LoweredStmt};
use crate::models::environment::Env;
use crate::models::result::ExecResult;
use crate::models::value::{Function, Value};

pub fn eval_expr(expr: &LoweredExpr, env: &mut Env, ctx: &mut Option<&mut MetaContext>) -> Value {
    match expr {
        LoweredExpr::Int(n) => Value::Int(*n),
        LoweredExpr::String(s) => Value::String(s.clone()),
        LoweredExpr::Bool(b) => Value::Bool(*b),
        LoweredExpr::Variable(name) => env
            .get(name)
            .unwrap_or_else(|| panic!("undefined variable: {}", name)),

        LoweredExpr::Add(a, b) => match (eval_expr(a, env, ctx), eval_expr(b, env, ctx)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
            _ => panic!("type error: + expects ints"),
        },

        LoweredExpr::Sub(a, b) => match (eval_expr(a, env, ctx), eval_expr(b, env, ctx)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
            _ => panic!("type error: - expects ints"),
        },

        LoweredExpr::Mult(a, b) => match (eval_expr(a, env, ctx), eval_expr(b, env, ctx)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
            _ => panic!("type error: * expects ints"),
        },

        LoweredExpr::Div(a, b) => match (eval_expr(a, env, ctx), eval_expr(b, env, ctx)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
            _ => panic!("type error: / expects ints"),
        },

        LoweredExpr::Equals(a, b) => match (eval_expr(a, env, ctx), eval_expr(b, env, ctx)) {
            (Value::Int(x), Value::Int(y)) => Value::Bool(x == y),
            (Value::String(x), Value::String(y)) => Value::Bool(x == y),
            (Value::Bool(x), Value::Bool(y)) => Value::Bool(x == y),
            _ => panic!("type error: == mismatched types"),
        },

        LoweredExpr::Call { callee, args } => {
            let callee_val = eval_expr(callee, env, ctx);
            let func = match callee_val {
                Value::Function(f) => f,
                _ => panic!("attempted to call a non-function"),
            };

            if func.params.len() != args.len() {
                panic!("wrong number of arguments");
            }

            let arg_vals: Vec<Value> = args.iter().map(|a| eval_expr(a, env, ctx)).collect();

            env.push_scope();

            for (param, value) in func.params.iter().zip(arg_vals) {
                env.set(param.clone(), value);
            }

            let result = match eval_stmt(&func.body, env, ctx) {
                ExecResult::Return(v) => v,
                ExecResult::Continue => Value::Unit,
            };

            env.pop_scope();

            result
        }
    }
}

pub fn eval_stmt(
    stmt: &LoweredStmt,
    env: &mut Env,
    ctx: &mut Option<&mut MetaContext>,
) -> ExecResult {
    match stmt {
        LoweredStmt::Print(expr) => {
            let value = eval_expr(expr, env, ctx);
            println!("{}", value);
            ExecResult::Continue
        }

        LoweredStmt::If {
            cond,
            body,
            else_branch,
        } => match eval_expr(cond, env, ctx) {
            Value::Bool(true) => eval_stmt(body, env, ctx),
            Value::Bool(false) => match else_branch {
                Some(else_stmt) => eval_stmt(else_stmt, env, ctx),
                None => ExecResult::Continue,
            },
            _ => panic!("type error: expected bool expr"),
        },

        LoweredStmt::ExprStmt(expr) => {
            eval_expr(expr, env, ctx);
            ExecResult::Continue
        }

        LoweredStmt::Assignment { name, expr } => {
            let value = eval_expr(expr, env, ctx);
            env.set(name.clone(), value);
            ExecResult::Continue
        }

        LoweredStmt::Block(stmts) => {
            env.push_scope();
            let res = eval(stmts, env, ctx);
            env.pop_scope();
            res
        }

        LoweredStmt::FnDecl { name, params, body } => {
            let func = Value::Function(Function {
                params: params.clone(),
                body: *body.clone(),
            });

            env.set(name.clone(), func);
            ExecResult::Continue
        }

        LoweredStmt::Return(opt_expr) => match opt_expr {
            None => ExecResult::Return(Value::Unit),
            Some(expr) => {
                let result = eval_expr(expr, env, ctx);
                ExecResult::Return(result)
            }
        },

        LoweredStmt::Gen(stmts) => {
            let meta = ctx.as_deref_mut().expect("gen outside meta");
            for stmt in stmts {
                meta.emitted.push(stmt.clone());
            }
            ExecResult::Continue
        }
    }
}

pub fn eval(
    stmts: &Vec<LoweredStmt>,
    env: &mut Env,
    ctx: &mut Option<&mut MetaContext>,
) -> ExecResult {
    for stmt in stmts {
        match eval_stmt(stmt, env, ctx) {
            ExecResult::Continue => {}
            ExecResult::Return(v) => {
                return ExecResult::Return(v);
            }
        }
    }
    ExecResult::Continue
}
