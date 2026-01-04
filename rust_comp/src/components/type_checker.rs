use crate::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use crate::models::semantics::typed_ast::{ToType, TypedExpr, TypedExprKind, TypedStmt};
use crate::models::types::type_env::TypeEnv;
use crate::models::types::type_error::TypeError;
use crate::models::types::type_subst::{unify, ApplySubst, TypeSubst};
use crate::models::types::types::*;

pub struct TypeCheckCtx {
    pub return_type: Option<Type>,
    pub saw_return: bool,
}

impl TypeCheckCtx {
    pub fn new() -> Self {
        Self {
            return_type: None,
            saw_return: false,
        }
    }
}

pub fn infer_expr_top(expr: &ExpandedExpr) -> Result<TypedExpr, TypeError> {
    infer_expr(expr, &mut TypeEnv::new(), &mut TypeSubst::new())
}

pub fn infer_expr(
    expr: &ExpandedExpr,
    env: &mut TypeEnv,
    subst: &mut TypeSubst,
) -> Result<TypedExpr, TypeError> {
    match expr {
        ExpandedExpr::Int(i) => Ok(TypedExpr {
            ty: int_type(),
            kind: TypedExprKind::Int(*i),
        }),
        ExpandedExpr::Bool(b) => Ok(TypedExpr {
            ty: bool_type(),
            kind: TypedExprKind::Bool(*b),
        }),
        ExpandedExpr::String(s) => Ok(TypedExpr {
            ty: string_type(),
            kind: TypedExprKind::String(s.clone()),
        }),
        ExpandedExpr::Variable(name) => {
            let ty = env
                .lookup(name)
                .ok_or(TypeError::UnboundVar(name.clone()))?;
            Ok(TypedExpr {
                ty,
                kind: TypedExprKind::Variable(name.clone()),
            })
        }
        ExpandedExpr::Call { callee, args } => {
            let callee_ty = env
                .lookup(callee)
                .ok_or(TypeError::UnboundVar(callee.clone()))?;

            let mut typed_args = Vec::new();
            let mut arg_types = Vec::new();
            for arg in args {
                let ta = infer_expr(arg, env, subst)?;
                arg_types.push(ta.ty.clone());
                typed_args.push(ta);
            }

            let ret_tv = Type::Var(env.fresh());

            let expected_fn = Type::Func {
                params: arg_types,
                ret: Box::new(ret_tv.clone()),
            };

            unify(&callee_ty, &expected_fn, subst)?;

            let result_ty = ret_tv.apply(subst);

            Ok(TypedExpr {
                ty: result_ty,
                kind: TypedExprKind::Call {
                    callee: callee.clone(),
                    args: typed_args,
                },
            })
        }
        _ => Err(TypeError::Unsupported),
    }
}

pub fn type_check_expr_top(expr: &ExpandedExpr, expected: &Type) -> Result<TypedExpr, TypeError> {
    type_check_expr(expr, &mut TypeEnv::new(), &mut TypeSubst::new(), expected)
}

pub fn type_check_expr(
    expr: &ExpandedExpr,
    env: &mut TypeEnv,
    subst: &mut TypeSubst,
    expected: &Type,
) -> Result<TypedExpr, TypeError> {
    let inferred_expr = infer_expr(expr, env, subst)?;
    let inferred_type = inferred_expr.to_type();
    unify(&inferred_type, expected, subst)?;
    if inferred_type == *expected {
        Ok(inferred_expr)
    } else {
        Err(TypeError::TypeMismatch {
            expected: expected.clone(),
            found: inferred_type,
        })
    }
}

pub fn infer_stmt_top(stmt: &ExpandedStmt) -> Result<TypedStmt, TypeError> {
    infer_stmt(
        stmt,
        &mut TypeEnv::new(),
        &mut TypeSubst::new(),
        &mut TypeCheckCtx::new(),
    )
}

pub fn infer_stmt(
    stmt: &ExpandedStmt,
    env: &mut TypeEnv,
    subst: &mut TypeSubst,
    ctx: &mut TypeCheckCtx,
) -> Result<TypedStmt, TypeError> {
    match stmt {
        ExpandedStmt::Assignment { name, expr } => {
            let typed_expr = infer_expr(expr, env, subst)?;
            env.bind_mono(name, typed_expr.ty.clone());
            let typed_assign = TypedStmt::Assignment {
                name: name.clone(),
                expr: Box::new(typed_expr),
            };
            Ok(typed_assign)
        }
        ExpandedStmt::Block(stmts) => {
            env.push_scope();
            let typed_stmts = infer_stmts(stmts, env, subst, ctx)?;
            env.pop_scope();
            let typed_block = TypedStmt::Block(typed_stmts);
            Ok(typed_block)
        }
        ExpandedStmt::If {
            cond,
            body,
            else_branch,
        } => {
            let typed_cond = type_check_expr(cond, env, subst, &bool_type())?;
            let typed_body = infer_stmt(body, env, subst, ctx)?;
            let typed_else = match else_branch {
                Some(el) => Some(Box::new(infer_stmt(el, env, subst, ctx)?)),
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
            let mut param_types = vec![];
            for _ in params {
                param_types.push(Type::Var(env.fresh()));
            }
            let ret_tv = Type::Var(env.fresh());

            let fn_type = Type::Func {
                params: param_types.clone(),
                ret: Box::new(ret_tv.clone()),
            };

            env.bind_mono(name, fn_type.clone());

            env.push_scope();
            for (param, ty) in params.iter().zip(param_types.iter()) {
                env.bind_mono(param, ty.clone());
            }

            let saved_ret = ctx.return_type.take();
            let saved_saw = ctx.saw_return;

            ctx.return_type = Some(ret_tv.clone());
            ctx.saw_return = false;

            let typed_body = infer_stmt(body, env, subst, ctx)?;

            if !ctx.saw_return {
                unify(&ret_tv, &Type::Primitive(PrimitiveType::Unit), subst)?;
            }

            ctx.return_type = saved_ret;
            ctx.saw_return = saved_saw;

            env.pop_scope();

            let final_fn_type = fn_type.apply(subst);
            env.bind_mono(name, final_fn_type);

            Ok(TypedStmt::FnDecl {
                name: name.clone(),
                params: params.clone(),
                body: Box::new(typed_body),
            })
        }

        ExpandedStmt::ExprStmt(expr) => {
            let typed_expr = infer_expr(expr, env, subst)?;
            Ok(TypedStmt::ExprStmt(Box::new(typed_expr)))
        }

        ExpandedStmt::Return(op_expr) => {
            let expr_ty = match op_expr {
                None => Type::Primitive(PrimitiveType::Unit),
                Some(expr) => infer_expr(expr, env, subst)?.ty,
            };

            let ret_ty = ctx.return_type.as_ref().ok_or(TypeError::InvalidReturn)?;

            ctx.saw_return = true;
            unify(&expr_ty, ret_ty, subst)?;

            Ok(TypedStmt::Return(
                op_expr
                    .as_ref()
                    .map(|e| Box::new(infer_expr(e, env, subst).unwrap())),
            ))
        }
        _ => Err(TypeError::Unsupported),
    }
}

pub fn infer_stmts(
    stmts: &Vec<ExpandedStmt>,
    env: &mut TypeEnv,
    subst: &mut TypeSubst,
    ctx: &mut TypeCheckCtx,
) -> Result<Vec<TypedStmt>, TypeError> {
    let mut stmt_vec = vec![];
    for stmt in stmts {
        let typed_stmt = infer_stmt(stmt, env, subst, ctx)?;
        stmt_vec.push(typed_stmt);
    }
    Ok(stmt_vec)
}

pub fn infer_stmts_top(stmts: &Vec<ExpandedStmt>) -> Result<Vec<TypedStmt>, TypeError> {
    infer_stmts(
        stmts,
        &mut TypeEnv::new(),
        &mut TypeSubst::new(),
        &mut TypeCheckCtx::new(),
    )
}
