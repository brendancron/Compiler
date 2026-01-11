use rust_comp::components::external_resolver::DummyResolver;
use rust_comp::components::pipeline::*;
use rust_comp::components::type_checker::*;
use rust_comp::models::decl_registry::DeclRegistry;
use rust_comp::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use rust_comp::models::types::type_env::TypeEnv;
use rust_comp::models::types::type_subst::TypeSubst;
use rust_comp::models::types::types::*;
use std::io;
use std::path::PathBuf;

#[cfg(test)]
mod type_check_tests {
    use super::*;

    fn exec_parse_pipeline(source: &str) -> Vec<ExpandedStmt> {
        let pipeline = lexer_pipeline()
            .then(parser_pipeline())
            .then(metaprocessor_pipeline(
                io::stdout(),
                DummyResolver{},
            ));

        let mut pipeline_ctx = PipelineCtx {
            out_dir: PathBuf::from("../out"),
            decl_reg: DeclRegistry::new(),
            root_dir: PathBuf::from("."),
        };

        pipeline.run(source.to_string(), &mut pipeline_ctx)
    }

    #[test]
    fn literals_are_typed_correctly() {
        let cases = vec![
            (ExpandedExpr::Int(42), int_type()),
            (ExpandedExpr::Bool(true), bool_type()),
            (ExpandedExpr::String("hi".into()), string_type()),
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
        env.bind_mono("x", int_type());

        let expr = ExpandedExpr::Variable("x".to_string());
        let typed = infer_expr(&expr, &mut env, &mut TypeSubst::new()).unwrap();

        assert_eq!(typed.ty, int_type());
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
        assert_eq!(env.lookup("x"), Some(int_type()));
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

        let typed = type_check_expr_top(&expr, &int_type()).unwrap();

        assert_eq!(typed.ty, int_type());
    }

    #[test]
    fn type_check_literal_mismatch_errors() {
        let expr = ExpandedExpr::Int(1);

        let result = type_check_expr_top(&expr, &bool_type());

        assert!(result.is_err());
    }

    #[test]
    fn type_check_variable_ok() {
        let mut env = TypeEnv::new();
        env.bind_mono("x", bool_type());

        let expr = ExpandedExpr::Variable("x".into());

        let typed = type_check_expr(&expr, &mut env, &mut TypeSubst::new(), &bool_type()).unwrap();

        assert_eq!(typed.ty, bool_type());
    }

    #[test]
    fn type_check_variable_mismatch_errors() {
        let mut env = TypeEnv::new();
        env.bind_mono("x", int_type());

        let expr = ExpandedExpr::Variable("x".into());

        let result = type_check_expr(&expr, &mut env, &mut TypeSubst::new(), &bool_type());

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
            env.lookup("foo"),
            Some(Type::Func {
                params: vec![],
                ret: Box::new(unit_type()),
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
            env.lookup("foo"),
            Some(Type::Func {
                params: vec![],
                ret: Box::new(int_type()),
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
            env.lookup("foo"),
            Some(Type::Func {
                params: vec![],
                ret: Box::new(int_type()),
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

    #[test]
    fn call_simple_function() {
        let source = "
            fn id(x) { return x; }
            id(3);
        ";

        let stmts = exec_parse_pipeline(source);

        infer_stmts_top(&stmts).unwrap();
    }

    #[test]
    fn call_returns_correct_type() {
        let source = "
            fn five() { return 5; }
            var x = five();
        ";

        let stmts = exec_parse_pipeline(source);

        let mut env = TypeEnv::new();
        let mut subst = TypeSubst::new();
        let mut ctx = TypeCheckCtx::new();

        infer_stmts(&stmts, &mut env, &mut subst, &mut ctx).unwrap();

        assert_eq!(env.lookup("x"), Some(int_type()));
    }

    #[test]
    fn call_with_multiple_params() {
        let source = "
            fn ret_first(a, b) { return a; }
            var x = ret_first(1, \"hi\");
        ";

        let stmts = exec_parse_pipeline(source);

        let mut env = TypeEnv::new();
        let mut subst = TypeSubst::new();
        let mut ctx = TypeCheckCtx::new();

        for stmt in stmts {
            infer_stmt(&stmt, &mut env, &mut subst, &mut ctx).unwrap();
        }

        assert_eq!(env.lookup("x"), Some(int_type()));
    }

    #[test]
    fn call_argument_mismatch_errors() {
        let source = "
            fn cond(x) {
                if(x) {
                    return 3;
                } else {
                    return 7;
                }
            }
            cond(3);
        ";

        let stmts = exec_parse_pipeline(source);

        let mut env = TypeEnv::new();
        let mut subst = TypeSubst::new();
        let mut ctx = TypeCheckCtx::new();

        assert!(infer_stmts(&stmts, &mut env, &mut subst, &mut ctx).is_err());
    }

    #[test]
    fn call_polymorphic_identity_twice() {
        let source = "
            fn id(x) { return x; }
            var a = id(1);
            var b = id(true);
        ";

        let stmts = exec_parse_pipeline(source);

        let mut env = TypeEnv::new();
        let mut subst = TypeSubst::new();
        let mut ctx = TypeCheckCtx::new();

        infer_stmts(&stmts, &mut env, &mut subst, &mut ctx).unwrap();

        assert_eq!(env.lookup("a"), Some(int_type()));
        assert_eq!(env.lookup("b"), Some(bool_type()));
    }

    #[test]
    fn struct_type_check() {
        let source = "
            struct Person {
                name: string;
                age: int
            }

            var p = Person {
                name: \"Alice\",
                age: 30
            };

            print(p);
        ";

        let stmts = exec_parse_pipeline(source);

        let mut env = TypeEnv::new();
        let mut subst = TypeSubst::new();
        let mut ctx = TypeCheckCtx::new();

        infer_stmts(&stmts, &mut env, &mut subst, &mut ctx).unwrap();
    }
}
