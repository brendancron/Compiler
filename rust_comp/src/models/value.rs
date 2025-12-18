use crate::models::ast::Function;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    String(String),
    Bool(bool),
    Unit,

    Function(Function),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Unit => write!(f, ""),
            _ => write!(f, ""),
        }
    }
}
