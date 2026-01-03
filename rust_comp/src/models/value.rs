use crate::models::environment::EnvRef;
use crate::models::semantics::expanded_ast::ExpandedStmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    String(String),
    Bool(bool),

    Struct {
        type_name: String,
        fields: Rc<RefCell<HashMap<String, Value>>>,
    },

    List(Rc<RefCell<Vec<Value>>>),

    Function(Rc<Function>),

    Unit,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Box<ExpandedStmt>,
    pub env: EnvRef,
}

impl Value {
    pub fn enumerate(&self) -> std::cell::Ref<'_, Vec<Value>> {
        match self {
            Value::List(list) => list.borrow(),
            _ => panic!("iterable expeced"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Unit => write!(f, ""),
            Value::List(list) => {
                let elems = list.borrow();
                write!(f, "[")?;
                for (i, v) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }
            Value::Struct { type_name, fields } => {
                let map = fields.borrow();
                write!(f, "{} {{", type_name)?;
                for (i, (k, v)) in map.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }

            _ => write!(f, ""),
        }
    }
}
