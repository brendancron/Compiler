use rust_comp::components::type_checker::{infer_expr, infer_stmt};
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
            let typed = infer_expr(&expr, &TypeEnv::new()).unwrap();
            assert_eq!(typed.ty, expected_ty);
        }
    }

    #[test]
    fn unsupported_expr_errors() {
        let expr = ExpandedExpr::Add(
            Box::new(ExpandedExpr::Int(1)),
            Box::new(ExpandedExpr::Int(2)),
        );
        assert!(infer_expr(&expr, &TypeEnv::new()).is_err());
    }

    #[test]
    fn variable_expr_errors() {
        let expr = ExpandedExpr::Variable("x".to_string());
        assert!(infer_expr(&expr, &TypeEnv::new()).is_err());
    }

    #[test]
    fn variable_expr_ok() {
        let mut env = TypeEnv::new();
        env.bind("x", Type::Primitive(PrimitiveType::Int));

        let expr = ExpandedExpr::Variable("x".to_string());
        let typed = infer_expr(&expr, &env).unwrap();

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
}
