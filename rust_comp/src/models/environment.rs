use crate::models::value::Value;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env {
    scopes: Vec<HashMap<String, Value>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }
}

impl Env {
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("cannot pop global scope");
    }
}

impl Env {
    pub fn get(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }
}

impl Env {
    pub fn set(&mut self, name: String, value: Value) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(&name) {
                scope.insert(name, value);
                return;
            }
        }

        self.scopes.last_mut().unwrap().insert(name, value);
    }
}

impl Env {
    pub fn new_call_env(&self) -> Self {
        Env {
            scopes: self.scopes.clone(),
        }
    }
}
