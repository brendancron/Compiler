use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug)]
pub enum SymbolKind {
    Function,
    Struct,
    Variable,
    Module,
}

#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub module: ModuleId,
}

#[derive(Debug)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub symbols: HashMap<String, SymbolId>,
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope {
            parent,
            symbols: HashMap::new(),
        });
        id
    }

    pub fn define(
        &mut self,
        scope: ScopeId,
        name: String,
        kind: SymbolKind,
        module: ModuleId,
    ) -> Result<SymbolId, String> {
        let scope_ref = &mut self.scopes[scope.0];

        if scope_ref.symbols.contains_key(&name) {
            return Err(format!("duplicate symbol `{}`", name));
        }

        let id = SymbolId(self.symbols.len());
        self.symbols.push(Symbol { name: name.clone(), kind, module });
        scope_ref.symbols.insert(name, id);

        Ok(id)
    }

    pub fn resolve(&self, scope: ScopeId, name: &str) -> Option<SymbolId> {
        let mut current = Some(scope);

        while let Some(sid) = current {
            let scope_ref = &self.scopes[sid.0];
            if let Some(sym) = scope_ref.symbols.get(name) {
                return Some(*sym);
            }
            current = scope_ref.parent;
        }

        None
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0]
    }
}
