use rust_comp::components::pipeline::PipelineBuilder;
use rust_comp::components::type_checker::{
    infer_expr, infer_expr_top, infer_stmt, infer_stmt_top, type_check_expr, type_check_expr_top,
    TypeCheckCtx,
};
use rust_comp::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use rust_comp::models::types::type_env::TypeEnv;
use rust_comp::models::types::type_subst::{unify, ApplySubst, TypeSubst};
use rust_comp::models::types::types::{PrimitiveType, Type, TypeVar};
use std::io;

#[cfg(test)]
mod type_check_tests {
    use super::*;

    fn exec_parse_pipeline(source: &str) -> Vec<ExpandedStmt> {
        let mut pipeline = PipelineBuilder::new()
            .with_lexer()
            .with_parser()
            .with_metaprocessor(io::stdout())
            .build();
        let res = pipeline.run(source.to_string());
        match res {
            (stmts, _) => stmts,
        }
    }

    #[test]
    fn literals_are_typed_correctly() {
        let cases = vec![
            (ExpandedExpr::Int(42), Type::Primitive(PrimitiveType::Int)),
            (
                ExpandedExpr::Bool(true),
                Type::Primitive(PrimitiveType::Bool),
            ),
            (
                ExpandedExpr::String("hi".into()),
                Type::Primitive(PrimitiveType::String),
            ),
        ];

        for (expr, expected_ty) in cases {
            let typed = infer_expr_top(&expr).unwrap();
            assert_eq!(typed.ty, expected_ty);
        }
    }

    #[test]
    fn variable_expr_errors() {
        let expr = ExpandedExpr::Variable("x".to_string());
        assert!(infer_expr_top(&expr).is_err());
    }

    #[test]
    fn variable_expr_ok() {
        let mut env = TypeEnv::new();
        env.bind("x", Type::Primitive(PrimitiveType::Int));

        let expr = ExpandedExpr::Variable("x".to_string());
        let typed = infer_expr(&expr, &mut env, &mut TypeSubst::new()).unwrap();

        assert_eq!(typed.ty, Type::Primitive(PrimitiveType::Int));
    }

    #[test]
    fn assignment_introduces_var() {
        let mut env = TypeEnv::new();
        let stmt = ExpandedStmt::Assignment {
            name: "x".into(),
            expr: Box::new(ExpandedExpr::Int(3)),
        };

        infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();
        assert_eq!(env.get_type("x"), Some(Type::Primitive(PrimitiveType::Int)));
    }

    #[test]
    fn block_scope_does_not_leak() {
        let mut env = TypeEnv::new();

        let block = ExpandedStmt::Block(vec![ExpandedStmt::Assignment {
            name: "x".into(),
            expr: Box::new(ExpandedExpr::Int(1)),
        }]);

        infer_stmt(
            &block,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();

        assert!(env.get_type("x").is_none());
    }

    #[test]
    fn type_check_literal_ok() {
        let expr = ExpandedExpr::Int(1);

        let typed = type_check_expr_top(&expr, &Type::Primitive(PrimitiveType::Int)).unwrap();

        assert_eq!(typed.ty, Type::Primitive(PrimitiveType::Int));
    }

    #[test]
    fn type_check_literal_mismatch_errors() {
        let expr = ExpandedExpr::Int(1);

        let result = type_check_expr_top(&expr, &Type::Primitive(PrimitiveType::Bool));

        assert!(result.is_err());
    }

    #[test]
    fn type_check_variable_ok() {
        let mut env = TypeEnv::new();
        env.bind("x", Type::Primitive(PrimitiveType::Bool));

        let expr = ExpandedExpr::Variable("x".into());

        let typed = type_check_expr(
            &expr,
            &mut env,
            &mut TypeSubst::new(),
            &Type::Primitive(PrimitiveType::Bool),
        )
        .unwrap();

        assert_eq!(typed.ty, Type::Primitive(PrimitiveType::Bool));
    }

    #[test]
    fn type_check_variable_mismatch_errors() {
        let mut env = TypeEnv::new();
        env.bind("x", Type::Primitive(PrimitiveType::Int));

        let expr = ExpandedExpr::Variable("x".into());

        let result = type_check_expr(
            &expr,
            &mut env,
            &mut TypeSubst::new(),
            &Type::Primitive(PrimitiveType::Bool),
        );

        assert!(result.is_err());
    }

    #[test]
    fn if_condition_must_be_bool() {
        let mut env = TypeEnv::new();

        let stmt = ExpandedStmt::If {
            cond: Box::new(ExpandedExpr::Int(1)),
            body: Box::new(ExpandedStmt::Block(vec![])),
            else_branch: None,
        };

        assert!(infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new()
        )
        .is_err());
    }

    #[test]
    fn if_bool_condition_ok() {
        let mut env = TypeEnv::new();

        let stmt = ExpandedStmt::If {
            cond: Box::new(ExpandedExpr::Bool(true)),
            body: Box::new(ExpandedStmt::Block(vec![])),
            else_branch: None,
        };

        infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();
    }

    #[test]
    fn if_branch_scope_does_not_leak() {
        let mut env = TypeEnv::new();

        let stmt = ExpandedStmt::If {
            cond: Box::new(ExpandedExpr::Bool(true)),
            body: Box::new(ExpandedStmt::Block(vec![ExpandedStmt::Assignment {
                name: "x".into(),
                expr: Box::new(ExpandedExpr::Int(1)),
            }])),
            else_branch: Some(Box::new(ExpandedStmt::Block(vec![
                ExpandedStmt::Assignment {
                    name: "y".into(),
                    expr: Box::new(ExpandedExpr::Bool(true)),
                },
            ]))),
        };

        infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();

        assert!(env.get_type("x").is_none());
        assert!(env.get_type("y").is_none());
    }

    #[test]
    fn if_without_else_ok() {
        let mut env = TypeEnv::new();

        let stmt = ExpandedStmt::If {
            cond: Box::new(ExpandedExpr::Bool(true)),
            body: Box::new(ExpandedStmt::Block(vec![])),
            else_branch: None,
        };

        infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();
    }

    #[test]
    fn fn_decl_zero_param_binds_empty_to_unit() {
        let mut env = TypeEnv::new();

        let stmt = ExpandedStmt::FnDecl {
            name: "foo".into(),
            params: vec![],
            body: Box::new(ExpandedStmt::Block(vec![])),
        };

        infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();

        assert_eq!(
            env.get_type("foo"),
            Some(Type::Func {
                params: vec![],
                ret: Box::new(Type::Primitive(PrimitiveType::Unit)),
            })
        );
    }

    #[test]
    fn fn_decl_zero_param_binds_empty_to_int() {
        let mut env = TypeEnv::new();

        let stmt = ExpandedStmt::FnDecl {
            name: "foo".into(),
            params: vec![],
            body: Box::new(ExpandedStmt::Block(vec![ExpandedStmt::Return(Some(
                Box::new(ExpandedExpr::Int(3)),
            ))])),
        };

        infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();

        assert_eq!(
            env.get_type("foo"),
            Some(Type::Func {
                params: vec![],
                ret: Box::new(Type::Primitive(PrimitiveType::Int)),
            })
        );
    }

    #[test]
    fn return_in_if_branches_match() {
        let source = "
            fn foo() {
                if (true) {
                    return 1;
                } else {
                    return 5;
                }
            }
        ";

        let stmt = exec_parse_pipeline(source)[0].clone();

        let mut env = TypeEnv::new();
        infer_stmt(
            &stmt,
            &mut env,
            &mut TypeSubst::new(),
            &mut TypeCheckCtx::new(),
        )
        .unwrap();

        assert_eq!(
            env.get_type("foo"),
            Some(Type::Func {
                params: vec![],
                ret: Box::new(Type::Primitive(PrimitiveType::Int)),
            })
        );
    }

    #[test]
    fn return_in_if_branches_mismatch_errors() {
        let source = "
            fn foo() {
                if (true) {
                    return 1;
                } else {
                    return true;
                }
            }
        ";

        let stmt = exec_parse_pipeline(source)[0].clone();

        let result = infer_stmt_top(&stmt);
        assert!(result.is_err());
    }
}

#[cfg(test)]
mod type_subst_tests {
    use super::*;

    fn tv(n: usize) -> Type {
        Type::Var(TypeVar { id: n })
    }

    fn int() -> Type {
        Type::Primitive(PrimitiveType::Int)
    }

    fn bool_() -> Type {
        Type::Primitive(PrimitiveType::Bool)
    }

    #[test]
    fn apply_substitution_simple() {
        let mut subst = TypeSubst::new();
        subst.map.insert(TypeVar { id: 0 }, int());

        let t = tv(0);
        assert_eq!(t.apply(&subst), int());
    }

    #[test]
    fn apply_substitution_recursive_func() {
        let mut subst = TypeSubst::new();
        subst.map.insert(TypeVar { id: 0 }, int());

        let t = Type::Func {
            params: vec![tv(0)],
            ret: Box::new(tv(0)),
        };

        let applied = t.apply(&subst);

        assert_eq!(
            applied,
            Type::Func {
                params: vec![int()],
                ret: Box::new(int()),
            }
        );
    }

    #[test]
    fn unify_var_with_primitive() {
        let mut subst = TypeSubst::new();

        unify(&tv(0), &int(), &mut subst).unwrap();

        assert_eq!(subst.map.get(&TypeVar { id: 0 }), Some(&int()));
    }

    #[test]
    fn unify_same_primitive() {
        let mut subst = TypeSubst::new();

        unify(&int(), &int(), &mut subst).unwrap();
        assert!(subst.map.is_empty());
    }

    #[test]
    fn unify_function_types() {
        let mut subst = TypeSubst::new();

        let f1 = Type::Func {
            params: vec![tv(0)],
            ret: Box::new(tv(0)),
        };

        let f2 = Type::Func {
            params: vec![int()],
            ret: Box::new(int()),
        };

        unify(&f1, &f2, &mut subst).unwrap();

        assert_eq!(subst.map.get(&TypeVar { id: 0 }), Some(&int()));
    }

    #[test]
    fn unify_mismatch_errors() {
        let mut subst = TypeSubst::new();

        let err = unify(&int(), &bool_(), &mut subst);
        assert!(err.is_err());
    }

    #[test]
    fn occurs_check_rejects_infinite_type() {
        let mut subst = TypeSubst::new();

        let t = tv(0);
        let bad = Type::Func {
            params: vec![tv(0)],
            ret: Box::new(int()),
        };

        let err = unify(&t, &bad, &mut subst);
        assert!(err.is_err());
    }
}
