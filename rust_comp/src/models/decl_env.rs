use crate::models::ast::TypeExpr;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct StructDef {
    pub fields: Vec<(String, TypeExpr)>,
}

#[derive(Clone)]
pub struct DeclEnv {
    structs: HashMap<String, StructDef>,
    parent: Option<Rc<DeclEnv>>,
}

impl DeclEnv {
    pub fn new() -> Rc<Self> {
        Rc::new(DeclEnv {
            structs: HashMap::new(),
            parent: None,
        })
    }

    pub fn child(parent: Rc<DeclEnv>) -> Rc<DeclEnv> {
        Rc::new(DeclEnv {
            structs: HashMap::new(),
            parent: Some(parent),
        })
    }

    pub fn define_struct(&mut self, name: String, def: StructDef) {
        self.structs.insert(name, def);
    }

    pub fn get_struct(&self, name: &str) -> Option<StructDef> {
        self.structs
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref()?.get_struct(name))
    }
}
