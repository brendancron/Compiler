use crate::models::symbols::symbol::*;
use crate::models::semantics::blueprint_ast::*;

pub fn declaration_pass(
    module_id: ModuleId,
    ast: &BlueprintAst,
    symbols: &mut SymbolTable,
) -> ScopeId {
    let module_scope = symbols.new_scope(None);
    for stmt in &ast.stmts {
        match stmt {
            BlueprintStmt::FnDecl {name, ..} => {
                symbols
                    .define(
                        module_scope,
                        name.clone(),
                        SymbolKind::Function,
                        module_id,
                    )
                    .unwrap();
            }

            BlueprintStmt::StructDecl { name, .. } => {
                symbols
                    .define(
                        module_scope,
                        name.clone(),
                        SymbolKind::Struct,
                        module_id,
                    )
                    .unwrap();
            }

            BlueprintStmt::Assignment { name, .. } => {
                symbols
                    .define(
                        module_scope,
                        name.clone(),
                        SymbolKind::Variable,
                        module_id,
                    )
                    .unwrap();
            }

            _ => {
                 // Not a top level decl ignore
            }
        }
    }
    module_scope
}
