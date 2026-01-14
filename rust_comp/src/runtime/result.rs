use crate::runtime::value::Value;

pub enum ExecResult {
    Continue,
    Return(Value),
}
