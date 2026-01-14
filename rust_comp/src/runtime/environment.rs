use super::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug, Clone)]
pub struct Env {
    scopes: Vec<HashMap<String, Value>>,
    parent: Option<EnvRef>,
}

impl Env {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Env {
            scopes: vec![HashMap::new()],
            parent: None,
        }))
    }

    pub fn new_child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Env {
            scopes: vec![HashMap::new()],
            parent: Some(parent),
        }))
    }
}

/* ---------------- Scope management (blocks) ---------------- */

impl Env {
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("cannot pop global scope");
    }
}

/* ---------------- Variable lookup ---------------- */

impl Env {
    pub fn get(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().get(name);
        }

        None
    }
}

/* ---------------- Variable definition vs assignment ---------------- */

impl Env {
    pub fn define(&mut self, name: String, value: Value) {
        self.scopes.last_mut().unwrap().insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(&name) {
                scope.insert(name, value);
                return;
            }
        }

        if let Some(parent) = &self.parent {
            parent.borrow_mut().assign(name, value);
            return;
        }

        panic!("assignment to undefined variable: {}", name);
    }
}

impl Env {
    pub fn flat_vars(&self) -> HashMap<String, Value> {
        let mut map = HashMap::new();

        if let Some(parent) = &self.parent {
            map.extend(parent.borrow().flat_vars());
        }

        for scope in &self.scopes {
            for (k, v) in scope {
                map.insert(k.clone(), v.clone());
            }
        }

        map
    }
}
