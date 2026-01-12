use super::pipeline::*;
use crate::models::semantics::BlueprintAst;
use crate::models::semantics::blueprint_module::{BlueprintModule, ModuleId};
use std::collections::HashMap;

pub type ModuleMap = HashMap<ModuleId, BlueprintModule>;
pub type PathMap = HashMap<String, ModuleId>;

pub fn load_modules(parse_pipeline: Pipeline<String, BlueprintAst>, root_path: String) -> (ModuleMap, ModuleId) {
    unimplemented!()
}

pub fn load_module(
    parse_pipeline: Pipeline<String, BlueprintAst>,
    modules: &mut ModuleMap,
    paths: &mut PathMap,
    current_path: String) -> ModuleId {
    unimplemented!()
}
