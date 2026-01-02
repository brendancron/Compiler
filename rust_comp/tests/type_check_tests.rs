use rust_comp::components::type_checker::lower_to_typed_expr;
use rust_comp::models::ast::ExpandedExpr;
use rust_comp::models::types::Type;
use std::collections::HashMap;

#[cfg(test)]
mod type_check_tests {
    use super::*;

    #[test]
    fn literals_are_typed_correctly() {
        let cases = vec![
            (ExpandedExpr::Int(42), Type::Int),
            (ExpandedExpr::Bool(true), Type::Bool),
            (ExpandedExpr::String("hi".into()), Type::String),
        ];

        for (expr, expected_ty) in cases {
            let typed = lower_to_typed_expr(&expr, &HashMap::new()).unwrap();
            assert_eq!(typed.ty, expected_ty);
        }
    }

    #[test]
    fn unsupported_expr_errors() {
        let expr = ExpandedExpr::Add(
            Box::new(ExpandedExpr::Int(1)),
            Box::new(ExpandedExpr::Int(2)),
        );
        assert!(lower_to_typed_expr(&expr, &HashMap::new()).is_err());
    }

    #[test]
    fn variable_expr_errors() {
        let expr = ExpandedExpr::Variable(String::from("x"));
        assert!(lower_to_typed_expr(&expr, &HashMap::new()).is_err());
    }
}
