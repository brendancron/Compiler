use crate::models::types::types::{Type, TypeVar};

#[derive(Debug, Clone)]
pub enum TypeError {
    InvalidReturn,
    Unsupported,
    UnboundVar(String),
    TypeMismatch { expected: Type, found: Type },
}
