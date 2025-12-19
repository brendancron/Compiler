use crate::models::ast::LoweredStmt;
use crate::models::environment::EnvRef;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    String(String),
    Bool(bool),
    Unit,

    Function(Rc<Function>),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: LoweredStmt,
    pub env: EnvRef,
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
