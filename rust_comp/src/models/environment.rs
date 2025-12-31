use crate::models::ast::TypeExpr;
use crate::models::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug, Clone)]
pub struct Env {
    scopes: Vec<HashMap<String, Value>>,
    structs: HashMap<String, StructDef>,
    parent: Option<EnvRef>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub fields: Vec<(String, TypeExpr)>,
}

impl Env {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Env {
            scopes: vec![HashMap::new()],
            structs: HashMap::new(),
            parent: None,
        }))
    }

    pub fn new_child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Env {
            scopes: vec![HashMap::new()],
            structs: HashMap::new(),
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
    pub fn define_struct(&mut self, name: String, def: StructDef) {
        self.structs.insert(name, def);
    }

    pub fn get_struct(&self, name: &str) -> Option<StructDef> {
        if let Some(def) = self.structs.get(name) {
            return Some(def.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().get_struct(name);
        }

        None
    }
}

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {{")?;
        for (name, ty) in &self.fields {
            writeln!(f, "    {}: {};", name, ty)?;
        }
        write!(f, "}}")
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
