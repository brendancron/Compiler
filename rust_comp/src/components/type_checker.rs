use crate::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use crate::models::semantics::typed_ast::{TypedExpr, TypedExprKind, TypedStmt};
use crate::models::type_env::TypeEnv;
use crate::models::types::{PrimitiveType, Type};

#[derive(Debug, Clone)]
pub enum TypeError {
    Unsupported,
    UnboundVar(String),
}

pub fn infer_expr(expr: &ExpandedExpr, env: &TypeEnv) -> Result<TypedExpr, TypeError> {
    match expr {
        ExpandedExpr::Int(i) => Ok(TypedExpr {
            ty: Type::Primitive(PrimitiveType::Int),
            kind: TypedExprKind::Int(*i),
        }),
        ExpandedExpr::Bool(b) => Ok(TypedExpr {
            ty: Type::Primitive(PrimitiveType::Bool),
            kind: TypedExprKind::Bool(*b),
        }),
        ExpandedExpr::String(s) => Ok(TypedExpr {
            ty: Type::Primitive(PrimitiveType::String),
            kind: TypedExprKind::String(s.clone()),
        }),
        ExpandedExpr::Variable(name) => {
            let ty = env
                .get_type(name.as_str())
                .ok_or(TypeError::UnboundVar(name.clone()))?;
            Ok(TypedExpr {
                ty,
                kind: TypedExprKind::Variable(name.clone()),
            })
        }
        _ => Err(TypeError::Unsupported),
    }
}

pub fn infer_stmt(stmt: &ExpandedStmt, env: &mut TypeEnv) -> Result<TypedStmt, TypeError> {
    match stmt {
        ExpandedStmt::Assignment { name, expr } => {
            let typed_expr = infer_expr(expr, env)?;
            env.bind(name, typed_expr.ty.clone());
            let typed_assign = TypedStmt::Assignment {
                name: name.clone(),
                expr: Box::new(typed_expr),
            };
            Ok(typed_assign)
        }
        ExpandedStmt::Block(stmts) => {
            env.push_scope();
            let typed_stmts = infer_stmts(stmts, env)?;
            env.pop_scope();
            let typed_block = TypedStmt::Block(typed_stmts);
            Ok(typed_block)
        }
        _ => Err(TypeError::Unsupported),
    }
}

pub fn infer_stmts(
    stmts: &Vec<ExpandedStmt>,
    env: &mut TypeEnv,
) -> Result<Vec<TypedStmt>, TypeError> {
    let mut stmt_vec = vec![];
    for stmt in stmts {
        let typed_stmt = infer_stmt(stmt, env)?;
        stmt_vec.push(typed_stmt);
    }
    Ok(stmt_vec)
}
