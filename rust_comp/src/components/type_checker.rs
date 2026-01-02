use crate::models::ast::ExpandedExpr;
use crate::models::typed_ast::{TypedExpr, TypedExprKind};
use crate::models::types::Type;

#[derive(Debug, Clone)]
pub struct TypeError;

pub fn lower_to_typed_expr(expr: &ExpandedExpr) -> Result<TypedExpr, TypeError> {
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
        _ => Err(TypeError),
    }
}
