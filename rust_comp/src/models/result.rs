use crate::models::value::Value;

pub enum ExecResult {
    Continue,
    Return(Value),
}
