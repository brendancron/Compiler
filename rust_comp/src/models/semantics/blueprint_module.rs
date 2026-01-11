use super::BlueprintAst;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug, Clone)]
pub struct BlueprintModule {
    pub id: ModuleId,
    pub ast: BlueprintAst,
    pub imports: Vec<ModuleId>,
}

