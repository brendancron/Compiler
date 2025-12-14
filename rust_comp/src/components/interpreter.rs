use crate::models::ast::{Expr, Stmt};
use crate::models::value::Value;

pub fn eval(stmt: &Stmt) {

    pub fn eval_expr(expr: &Expr) -> Value {
        match expr {
            Expr::Int(n) => Value::Int(*n),
            Expr::String(s) => Value::String(s.clone()),
            Expr::Bool(b) => Value::Bool(*b),

            Expr::Add(a, b) => match (eval_expr(a), eval_expr(b)) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
                _ => panic!("type error: + expects ints"),
            },

            Expr::Sub(a, b) => match (eval_expr(a), eval_expr(b)) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
                _ => panic!("type error: - expects ints"),
            },

            Expr::Mult(a, b) => match (eval_expr(a), eval_expr(b)) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
                _ => panic!("type error: * expects ints"),
            },

            Expr::Div(a, b) => match (eval_expr(a), eval_expr(b)) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
                _ => panic!("type error: / expects ints"),
            },

            Expr::Equals(a, b) => match (eval_expr(a), eval_expr(b)) {
                (Value::Int(x), Value::Int(y)) => Value::Bool(x == y),
                (Value::String(x), Value::String(y)) => Value::Bool(x == y),
                (Value::Bool(x), Value::Bool(y)) => Value::Bool(x == y),
                _ => panic!("type error: == mismatched types"),
            },
        }
    }

    pub fn eval_stmt(stmt: &Stmt) {
        match stmt {
            Stmt::ExprStmt(expr) => {
                eval_expr(&expr);
            }

            Stmt::PrintStmt(expr) => {
                let value = eval_expr(expr);
                println!("{}", value);
            }
        }
    }

    eval_stmt(&stmt);
}
