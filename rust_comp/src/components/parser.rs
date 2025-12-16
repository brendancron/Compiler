use crate::models::ast::Expr;
use crate::models::token::{Token, TokenMetadata, TokenType};

fn peek<'a>(tokens: &'a [Token], pos: usize) -> Option<&'a Token> {
    tokens.get(pos)
}

fn consume<'a>(tokens: &'a [Token], pos: &mut usize, expected: TokenType) -> Option<&'a Token> {
    match tokens.get(*pos) {
        Some(t) if t.token_type == expected => consume_next(tokens, pos),
        _ => panic!("expected {:?}", expected),
    }
}

fn consume_next<'a>(tokens: &'a [Token], pos: &mut usize) -> Option<&'a Token> {
    let tok = tokens.get(*pos)?;
    *pos += 1;
    println!("Consumed: {:?}", tok);
    Some(tok)
}

pub fn parse(tokens: &[Token]) -> Expr {
    let mut pos = 0;

    fn parse_factor<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
        match tokens.get(*pos) {
            Some(tok) => match (tok.token_type) {
                TokenType::Number => {
                    consume_next(tokens, pos);
                    Expr::Int(tok.expect_int())
                }

                TokenType::String => {
                    consume_next(tokens, pos);
                    Expr::String(tok.expect_str())
                }

                TokenType::True => {
                    consume_next(tokens, pos);
                    Expr::Bool(true)
                }

                TokenType::False => {
                    consume_next(tokens, pos);
                    Expr::Bool(false)
                }

                TokenType::LeftParen => {
                    consume(tokens, pos, TokenType::LeftParen);
                    let expr = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    expr
                }

                _ => panic!("expected literal or '('"),
            },
            None => panic!("Unexpected EOF"),
        }
    }

    fn parse_term<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
        let mut left = parse_factor(tokens, pos);

        loop {
            match tokens.get(*pos) {
                Some(tok) => match tok.token_type {
                    TokenType::Star => {
                        *pos += 1;
                        let right = parse_factor(tokens, pos);
                        left = Expr::Mult(Box::new(left), Box::new(right));
                    }
                    TokenType::Slash => {
                        *pos += 1;
                        let right = parse_factor(tokens, pos);
                        left = Expr::Div(Box::new(left), Box::new(right));
                    }
                    _ => return left,
                },
                _ => return left,
            }
        }
    }

    fn parse_expr<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
        let mut left = parse_term(tokens, pos);

        loop {
            match tokens.get(*pos) {
                Some(tok) => match tok.token_type {
                    TokenType::Plus => {
                        *pos += 1;
                        let right = parse_term(tokens, pos);
                        left = Expr::Add(Box::new(left), Box::new(right));
                    }

                    TokenType::Minus => {
                        *pos += 1;
                        let right = parse_term(tokens, pos);
                        left = Expr::Equals(Box::new(left), Box::new(right));
                    }

                    TokenType::EqualEqual => {
                        *pos += 1;
                        let right = parse_term(tokens, pos);
                        left = Expr::Sub(Box::new(left), Box::new(right));
                    }

                    _ => return left,
                },
                _ => return left,
            }
        }
    }

    fn parse_keyword<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
        match tokens.get(*pos) {
            Some(tok) => match tok.token_type {
                TokenType::Print => {
                    consume(tokens, pos, TokenType::Print);
                    consume(tokens, pos, TokenType::LeftParen);
                    let expr = parse_keyword(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::Semicolon);
                    Expr::Print(Box::new(expr))
                }

                TokenType::If => {
                    consume(tokens, pos, TokenType::If);
                    consume(tokens, pos, TokenType::LeftParen);
                    let conditional = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::LeftBrace);
                    let inner = parse_keyword(tokens, pos);
                    consume(tokens, pos, TokenType::RightBrace);
                    Expr::If(Box::new(conditional), Box::new(inner))
                }

                TokenType::Var => {
                    consume(tokens, pos, TokenType::Var);
                    consume(tokens, pos, TokenType::Identifier);
                    consume(tokens, pos, TokenType::Equal);
                    let expr = parse_keyword(tokens, pos);
                    consume(tokens, pos, TokenType::Semicolon);
                    Expr::Assignment(tok.expect_str(), Box::new(expr))
                }

                _ => parse_expr(tokens, pos),
            },
            _ => parse_expr(tokens, pos),
        }
    }

    parse_keyword(tokens, &mut pos)
}
