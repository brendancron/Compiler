use crate::models::ast::ExpandedExpr;
use crate::models::typed_ast::{TypedExpr, TypedExprKind};
use crate::models::types::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum TypeError {
    Unsupported,
    UnboundVar(String),
}

pub type TypeEnv = HashMap<String, Type>;

pub fn lower_to_typed_expr(expr: &ExpandedExpr, env: &TypeEnv) -> Result<TypedExpr, TypeError> {
    match expr {
        ExpandedExpr::Int(i) => Ok(TypedExpr {
            ty: Type::Int,
            kind: TypedExprKind::Int(*i),
        }),
        ExpandedExpr::Bool(b) => Ok(TypedExpr {
            ty: Type::Bool,
            kind: TypedExprKind::Bool(*b),
        }),
        ExpandedExpr::String(s) => Ok(TypedExpr {
            ty: Type::String,
            kind: TypedExprKind::String(s.clone()),
        }),
        ExpandedExpr::Variable(name) => {
            let ty = env
                .get(name)
                .cloned()
                .ok_or(TypeError::UnboundVar(name.clone()))?;
            Ok(TypedExpr {
                ty,
                kind: TypedExprKind::Variable(name.clone()),
            })
        }
        _ => Err(TypeError::Unsupported),
    }
}
