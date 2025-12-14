use crate::models::ast::Expr;
use crate::models::value::Value;

pub fn eval(expr: &Expr) -> Value {
    match expr {
        Expr::Int(n) => Value::Int(*n),
        Expr::String(s) => Value::String(s.clone()),
        Expr::Bool(b) => Value::Bool(*b),

        Expr::Add(a, b) => match (eval(a), eval(b)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
            _ => panic!("type error: + expects ints"),
        },

        Expr::Sub(a, b) => match (eval(a), eval(b)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
            _ => panic!("type error: - expects ints"),
        },

        Expr::Mult(a, b) => match (eval(a), eval(b)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
            _ => panic!("type error: * expects ints"),
        },

        Expr::Div(a, b) => match (eval(a), eval(b)) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
            _ => panic!("type error: / expects ints"),
        },

        Expr::Equals(a, b) => match (eval(a), eval(b)) {
            (Value::Int(x), Value::Int(y)) => Value::Bool(x == y),
            (Value::String(x), Value::String(y)) => Value::Bool(x == y),
            (Value::Bool(x), Value::Bool(y)) => Value::Bool(x == y),
            _ => panic!("type error: == mismatched types"),
        },

        Expr::Print(expr) => {
            let value = eval(expr);
            println!("{}", value);
            Value::None
        }

        Expr::If(cond, inner) => match eval(cond) {
            Value::Bool(b) => {
                if b {
                    eval(inner)
                } else {
                    Value::None
                }
            }
            _ => panic!("type error: expected bool expr"),
        },

        _ => {
            panic!("Expression not handled!");
        }
    }
}
