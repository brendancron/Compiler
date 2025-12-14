use crate::models::token::Token;
use crate::models::ast::Expr;
use crate::models::ast::Stmt;

pub fn parse(tokens: &[Token]) -> Stmt {
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
    
    fn parse_stmt<'a>(
        tokens: &'a [Token],
        pos: &mut usize,
    ) -> Stmt {
        match tokens.get(*pos) {
            Some(Token::Print) => {
                *pos += 1; // consume 'print'
                match tokens.get(*pos) {
                    Some(Token::LeftParen) => {
                        *pos += 1; // consume '('
                        let expr = parse_expr(tokens, pos);
                        match tokens.get(*pos) {
                            Some(Token::RightParen) => {
                                *pos += 1; // consume ')'
                                match tokens.get(*pos) {
                                    Some(Token::Semicolon) => {
                                        *pos += 1; // consume ';'
                                        Stmt::PrintStmt(Box::new(expr))
                                    }
                                    _ => panic!("expected ';' after print statement"),
                                }
                            }
                            _ => panic!("expected ')' after print expression"),
                        }
                    }
                    _ => panic!("expected '(' after 'print'"),
                }
            }
            _ => {
                let expr = parse_expr(tokens, pos);
                Stmt::ExprStmt(Box::new(expr))
            }
        }
    }

    parse_stmt(tokens, &mut pos)
}
