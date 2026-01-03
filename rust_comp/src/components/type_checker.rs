use crate::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use crate::models::semantics::typed_ast::{ToType, TypedExpr, TypedExprKind, TypedStmt};
use crate::models::type_env::TypeEnv;
use crate::models::types::{PrimitiveType, Type};

#[derive(Debug, Clone)]
pub enum TypeError {
    InvalidReturn,
    Unsupported,
    UnboundVar(String),
    TypeMismatch { expected: Type, found: Type },
}

pub struct TypeCheckCtx {
    pub return_type: Option<Type>,
    pub in_function: bool,
}

impl TypeCheckCtx {
    pub fn new() -> Self {
        Self {
            return_type: None,
            in_function: false,
        }
    }

    pub fn with_function_scope<T>(
        &mut self,
        f: impl FnOnce(&mut TypeCheckCtx) -> Result<T, TypeError>,
    ) -> Result<(T, Option<Type>), TypeError> {
        let saved = self.return_type.take();
        let saved_in_fn = self.in_function;

        self.return_type = None;
        self.in_function = true;

        let result = f(self)?;

        let ret = self.return_type.take();

        self.return_type = saved;
        self.in_function = saved_in_fn;

        Ok((result, ret))
    }
}

pub fn infer_expr(expr: &ExpandedExpr, env: &mut TypeEnv) -> Result<TypedExpr, TypeError> {
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

pub fn type_check_expr(
    expr: &ExpandedExpr,
    env: &mut TypeEnv,
    expected: &Type,
) -> Result<TypedExpr, TypeError> {
    let inferred_expr = infer_expr(expr, env)?;
    let inferred_type = inferred_expr.to_type();
    if inferred_type == *expected {
        Ok(inferred_expr)
    } else {
        Err(TypeError::TypeMismatch {
            expected: expected.clone(),
            found: inferred_type,
        })
    }
}

pub fn infer_stmt(
    stmt: &ExpandedStmt,
    env: &mut TypeEnv,
    ctx: &mut TypeCheckCtx,
) -> Result<TypedStmt, TypeError> {
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
            let typed_stmts = infer_stmts(stmts, env, ctx)?;
            env.pop_scope();
            let typed_block = TypedStmt::Block(typed_stmts);
            Ok(typed_block)
        }
        ExpandedStmt::If {
            cond,
            body,
            else_branch,
        } => {
            let typed_cond = type_check_expr(cond, env, &Type::Primitive(PrimitiveType::Bool))?;
            let typed_body = infer_stmt(body, env, ctx)?;
            let typed_else = match else_branch {
                Some(el) => Some(Box::new(infer_stmt(el, env, ctx)?)),
                None => None,
            };
            let typed_if = TypedStmt::If {
                cond: Box::new(typed_cond),
                body: Box::new(typed_body),
                else_branch: typed_else,
            };
            Ok(typed_if)
        }
        ExpandedStmt::FnDecl { name, params, body } => {
            env.push_scope();
            let mut param_types = vec![];
            for param in params {
                let tv = Type::Var(env.fresh());
                env.bind(param, tv.clone());
                param_types.push(tv);
            }
            let (typed_body, ret_ty) = ctx.with_function_scope(|ctx| infer_stmt(body, env, ctx))?;
            env.pop_scope();
            let fn_type = Type::Func {
                params: param_types,
                ret: Box::new(ret_ty.unwrap_or(Type::Primitive(PrimitiveType::Unit))),
            };
            env.bind(name, fn_type.clone());
            Ok(TypedStmt::FnDecl {
                name: name.clone(),
                params: params.clone(),
                body: Box::new(typed_body),
            })
        }
        ExpandedStmt::Return(op_expr) => {
            let expr_ty = match op_expr {
                None => Type::Primitive(PrimitiveType::Unit),
                Some(expr) => infer_expr(expr, env)?.ty,
            };

            match &ctx.return_type {
                None => {
                    ctx.return_type = Some(expr_ty.clone());
                }
                Some(existing) => {
                    if *existing != expr_ty {
                        return Err(TypeError::TypeMismatch {
                            expected: existing.clone(),
                            found: expr_ty,
                        });
                    }
                }
            }

            Ok(TypedStmt::Return(
                op_expr
                    .as_ref()
                    .map(|e| Box::new(infer_expr(e, env).unwrap())),
            ))
        }
        _ => Err(TypeError::Unsupported),
    }
}

pub fn infer_stmts(
    stmts: &Vec<ExpandedStmt>,
    env: &mut TypeEnv,
    ctx: &mut TypeCheckCtx,
) -> Result<Vec<TypedStmt>, TypeError> {
    let mut stmt_vec = vec![];
    for stmt in stmts {
        let typed_stmt = infer_stmt(stmt, env, ctx)?;
        stmt_vec.push(typed_stmt);
    }
    Ok(stmt_vec)
}
