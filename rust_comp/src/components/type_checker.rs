use crate::models::ast::{ExpandedExpr, ExpandedStmt};
use crate::models::typed_ast::{ToType, TypedExpr, TypedExprKind, TypedStmt, TypedStmtKind};
use crate::models::types::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum TypeError {
    Unsupported,
    UnboundVar(String),
}

pub type TypeEnv = HashMap<String, Type>;

pub fn infer_expr(expr: &ExpandedExpr, env: &TypeEnv) -> Result<TypedExpr, TypeError> {
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

pub fn infer_stmt(stmt: ExpandedStmt, env: &TypeEnv) -> Result<TypedStmt, TypeError> {
    match stmt {
        ExpandedStmt::Assignment { name, expr } => type_check_assignment(env, &name, &expr),
        _ => Err(TypeError::Unsupported),
    }
}

fn type_check_assignment(
    env: &TypeEnv,
    name: &String,
    expr: &ExpandedExpr,
) -> Result<TypedStmt, TypeError> {
    let typed_expr = infer_expr(expr, env)?;
    let ty = typed_expr.to_type();
    let scheme = generalize(&typed_expr.ty, env);
    env.insert(name.clone(), scheme);
    let kind = TypedStmtKind::Assignment {
        name: name.clone(),
        expr: Box::new(typed_expr),
    };
    Ok(TypedStmt { ty, kind })
}
