use crate::models::ast::{Expr, Function, Stmt};
use crate::models::environment::Env;
use crate::models::value::Value;

pub fn eval_expr(expr: &Expr, env: &mut Env) -> Value {
    match expr {
        Expr::Int(n) => Value::Int(*n),
        Expr::String(s) => Value::String(s.clone()),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Variable(name) => env
            .get(name)
            .unwrap_or_else(|| panic!("undefined variable: {}", name)),

        Expr::Add(a, b) => match (eval_expr(a, env), eval_expr(b, env)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
            _ => panic!("type error: + expects ints"),
        },

        Expr::Sub(a, b) => match (eval_expr(a, env), eval_expr(b, env)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
            _ => panic!("type error: - expects ints"),
        },

        Expr::Mult(a, b) => match (eval_expr(a, env), eval_expr(b, env)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
            _ => panic!("type error: * expects ints"),
        },

        Expr::Div(a, b) => match (eval_expr(a, env), eval_expr(b, env)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
            _ => panic!("type error: / expects ints"),
        },

        Expr::Equals(a, b) => match (eval_expr(a, env), eval_expr(b, env)) {
            (Value::Int(x), Value::Int(y)) => Value::Bool(x == y),
            (Value::String(x), Value::String(y)) => Value::Bool(x == y),
            (Value::Bool(x), Value::Bool(y)) => Value::Bool(x == y),
            _ => panic!("type error: == mismatched types"),
        },

        Expr::Call { callee, args } => {
            let callee_val = eval_expr(callee, env);
            let func = match callee_val {
                Value::Function(f) => f,
                _ => panic!("attempted to call a non-function"),
            };

            if func.params.len() != args.len() {
                panic!("wrong number of arguments");
            }

            let arg_vals: Vec<Value> = args.iter().map(|a| eval_expr(a, env)).collect();

            env.push_scope();

            for (param, value) in func.params.iter().zip(arg_vals) {
                env.set(param.clone(), value);
            }

            eval_stmt(&func.body, env);

            env.pop_scope();

            Value::None
        }
    }
}

pub fn eval_stmt(stmt: &Stmt, env: &mut Env) {
    match stmt {
        Stmt::Print(expr) => {
            let value = eval_expr(expr, env);
            println!("{}", value);
        }

        Stmt::If { cond, body } => match eval_expr(cond, env) {
            Value::Bool(b) => {
                if b {
                    eval_stmt(body, env)
                }
            }
            _ => panic!("type error: expected bool expr"),
        },

        Stmt::ExprStmt(expr) => {
            eval_expr(expr, env);
        }

        Stmt::Assignment { name, expr } => {
            let value = eval_expr(expr, env);
            env.set(name.clone(), value);
        }

        Stmt::Block(stmts) => {
            env.push_scope();
            for stmt in stmts {
                eval_stmt(stmt, env);
            }
            env.pop_scope();
        }

        Stmt::FnDecl { name, params, body } => {
            let func = Value::Function(Function {
                params: params.clone(),
                body: *body.clone(),
            });

            env.set(name.clone(), func);
        }
    }
}
