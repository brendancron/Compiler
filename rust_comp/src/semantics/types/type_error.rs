use super::types::Type;

#[derive(Debug, Clone)]
pub enum TypeError {
    InvalidReturn,
    Unsupported,
    UnboundVar(String),
    TypeMismatch { expected: Type, found: Type },
}
