use rust_comp::components::type_checker::{infer_expr, infer_stmt, type_check_expr};
use rust_comp::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use rust_comp::models::type_env::TypeEnv;
use rust_comp::models::types::{PrimitiveType, Type};

#[cfg(test)]
mod type_check_tests {
    use super::*;

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
            let typed = infer_expr(&expr, &mut TypeEnv::new()).unwrap();
            assert_eq!(typed.ty, expected_ty);
        }
    }

    #[test]
    fn variable_expr_errors() {
        let expr = ExpandedExpr::Variable("x".to_string());
        assert!(infer_expr(&expr, &mut TypeEnv::new()).is_err());
    }

    #[test]
    fn variable_expr_ok() {
        let mut env = TypeEnv::new();
        env.bind("x", Type::Primitive(PrimitiveType::Int));

        let expr = ExpandedExpr::Variable("x".to_string());
        let typed = infer_expr(&expr, &mut env).unwrap();

        assert_eq!(typed.ty, Type::Primitive(PrimitiveType::Int));
    }

    #[test]
    fn assignment_introduces_var() {
        let mut env = TypeEnv::new();
        let stmt = ExpandedStmt::Assignment {
            name: "x".into(),
            expr: Box::new(ExpandedExpr::Int(3)),
        };

        infer_stmt(&stmt, &mut env).unwrap();
        assert_eq!(env.get_type("x"), Some(Type::Primitive(PrimitiveType::Int)));
    }

    #[test]
    fn block_scope_does_not_leak() {
        let mut env = TypeEnv::new();

        let block = ExpandedStmt::Block(vec![ExpandedStmt::Assignment {
            name: "x".into(),
            expr: Box::new(ExpandedExpr::Int(1)),
        }]);

        infer_stmt(&block, &mut env).unwrap();

        assert!(env.get_type("x").is_none());
    }

    #[test]
    fn type_check_literal_ok() {
        let mut env = TypeEnv::new();
        let expr = ExpandedExpr::Int(1);

        let typed = type_check_expr(&expr, &mut env, &Type::Primitive(PrimitiveType::Int)).unwrap();

        assert_eq!(typed.ty, Type::Primitive(PrimitiveType::Int));
    }

    #[test]
    fn type_check_literal_mismatch_errors() {
        let mut env = TypeEnv::new();
        let expr = ExpandedExpr::Int(1);

        let result = type_check_expr(&expr, &mut env, &Type::Primitive(PrimitiveType::Bool));

        assert!(result.is_err());
    }

    #[test]
    fn type_check_variable_ok() {
        let mut env = TypeEnv::new();
        env.bind("x", Type::Primitive(PrimitiveType::Bool));

        let expr = ExpandedExpr::Variable("x".into());

        let typed =
            type_check_expr(&expr, &mut env, &Type::Primitive(PrimitiveType::Bool)).unwrap();

        assert_eq!(typed.ty, Type::Primitive(PrimitiveType::Bool));
    }

    #[test]
    fn type_check_variable_mismatch_errors() {
        let mut env = TypeEnv::new();
        env.bind("x", Type::Primitive(PrimitiveType::Int));

        let expr = ExpandedExpr::Variable("x".into());

        let result = type_check_expr(&expr, &mut env, &Type::Primitive(PrimitiveType::Bool));

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

        assert!(infer_stmt(&stmt, &mut env).is_err());
    }

    #[test]
    fn if_bool_condition_ok() {
        let mut env = TypeEnv::new();

        let stmt = ExpandedStmt::If {
            cond: Box::new(ExpandedExpr::Bool(true)),
            body: Box::new(ExpandedStmt::Block(vec![])),
            else_branch: None,
        };

        infer_stmt(&stmt, &mut env).unwrap();
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

        infer_stmt(&stmt, &mut env).unwrap();

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

        infer_stmt(&stmt, &mut env).unwrap();
    }
}
