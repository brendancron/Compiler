use crate::models::token::Token;
use crate::models::ast::Expr;

pub fn parse(tokens: &[Token]) -> Expr {
    let mut pos = 0;

    // factor := NUMBER | STRING | TRUE | FALSE | "(" expr ")"
    fn parse_factor<'a>(
        tokens: &'a [Token],
        pos: &mut usize,
    ) -> Expr {
        match tokens.get(*pos).unwrap_or(&Token::EOF) {
            Token::Number(n) => {
                *pos += 1;
                Expr::Int(*n)
            }

            Token::String(s) => {
                *pos += 1;
                Expr::String(s.clone())
            }

            Token::True => {
                *pos += 1;
                Expr::Bool(true)
            }

            Token::False => {
                *pos += 1;
                Expr::Bool(false)
            }

            Token::LeftParen => {
                *pos += 1; // (
                let expr = parse_expr(tokens, pos);
                match tokens.get(*pos) {
                    Some(Token::RightParen) => {
                        *pos += 1;
                    }
                    _ => panic!("expected ')'"),
                }
                expr
            }

            _ => panic!("expected literal or '('"),
        }
    }

    // term := factor (("*" factor) | ("/" factor))*
    fn parse_term<'a>(
        tokens: &'a [Token],
        pos: &mut usize,
    ) -> Expr {
        let mut left = parse_factor(tokens, pos);

        loop {
            match tokens.get(*pos) {
                Some(Token::Star) => {
                    *pos += 1;
                    let right = parse_factor(tokens, pos);
                    left = Expr::Mult(Box::new(left), Box::new(right));
                }
                Some(Token::Slash) => {
                    *pos += 1;
                    let right = parse_factor(tokens, pos);
                    left = Expr::Div(Box::new(left), Box::new(right));
                }
                _ => return left,
            }
        }
    }

    // expr := term (("+" term) | ("-" term))*
    fn parse_expr<'a>(
        tokens: &'a [Token],
        pos: &mut usize,
    ) -> Expr {
        let mut left = parse_term(tokens, pos);

        loop {
            match tokens.get(*pos) {
                Some(Token::Plus) => {
                    *pos += 1;
                    let right = parse_term(tokens, pos);
                    left = Expr::Add(Box::new(left), Box::new(right));
                }
                Some(Token::Minus) => {
                    *pos += 1;
                    let right = parse_term(tokens, pos);
                    left = Expr::Sub(Box::new(left), Box::new(right));
                }
                Some(Token::EqualEqual) => {
                    *pos += 1;
                    let right = parse_term(tokens, pos);
                    left = Expr::Equals(Box::new(left), Box::new(right));
                }
                _ => return left,
            }
        }
    }

    parse_expr(tokens, &mut pos)
}
