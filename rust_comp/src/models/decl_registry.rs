use crate::models::semantics::blueprint_ast::TypeExpr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct StructDef {
    pub fields: Vec<(String, TypeExpr)>,
}

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {{")?;
        for (name, ty) in &self.fields {
            writeln!(f, "  {}: {};", name, ty)?;
        }
        write!(f, "}}")
    }
}

pub type DeclRegistryRef = Rc<RefCell<DeclRegistry>>;

#[derive(Clone)]
pub struct DeclRegistry {
    structs: HashMap<String, StructDef>,
    parent: Option<DeclRegistryRef>,
}

impl DeclRegistry {
    pub fn new() -> DeclRegistryRef {
        Rc::new(RefCell::new(DeclRegistry {
            structs: HashMap::new(),
            parent: None,
        }))
    }

    pub fn child(parent: DeclRegistryRef) -> DeclRegistryRef {
        Rc::new(RefCell::new(DeclRegistry {
            structs: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn define_struct(&mut self, name: String, def: StructDef) {
        self.structs.insert(name, def);
    }

    pub fn get_struct(&self, name: &str) -> Option<StructDef> {
        self.structs.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.borrow().get_struct(name))
        })
    }
}
