use crate::models::ast::{Expr, Stmt};
use crate::models::token::{Token, TokenType};

fn consume<'a>(tokens: &'a [Token], pos: &mut usize, expected: TokenType) -> &'a Token {
    match tokens.get(*pos) {
        Some(t) if t.token_type == expected => consume_next(tokens, pos),
        Some(t) => panic!(
            "expected {:?}, found {:?} at position {}",
            expected, t.token_type, pos
        ),
        None => panic!("expected {:?}, found EOF at position {}", expected, pos),
    }
}

fn consume_next<'a>(tokens: &'a [Token], pos: &mut usize) -> &'a Token {
    let tok = tokens
        .get(*pos)
        .expect("internal error: consume_next out of bounds");
    *pos += 1;
    println!("Consumed: {:?}", tok);
    tok
}

pub fn parse(tokens: &[Token]) -> Stmt {
    let mut pos = 0;

    fn parse_factor<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
        match tokens.get(*pos) {
            Some(tok) => match tok.token_type {
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

                TokenType::Identifier => {
                    consume_next(tokens, pos);
                    Expr::Variable(tok.expect_str())
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

    fn parse_stmt<'a>(tokens: &'a [Token], pos: &mut usize) -> Stmt {
        match tokens.get(*pos) {
            Some(tok) => match tok.token_type {
                TokenType::Print => {
                    consume(tokens, pos, TokenType::Print);
                    consume(tokens, pos, TokenType::LeftParen);
                    let expr = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::Semicolon);
                    Stmt::Print(Box::new(expr))
                }

                TokenType::If => {
                    consume(tokens, pos, TokenType::If);
                    consume(tokens, pos, TokenType::LeftParen);
                    let conditional = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::LeftBrace);
                    let inner = parse_stmt(tokens, pos);
                    consume(tokens, pos, TokenType::RightBrace);
                    Stmt::If(Box::new(conditional), Box::new(inner))
                }

                TokenType::Var => {
                    consume(tokens, pos, TokenType::Var);
                    let id = consume(tokens, pos, TokenType::Identifier);
                    consume(tokens, pos, TokenType::Equal);
                    let expr = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::Semicolon);
                    Stmt::Assignment(id.expect_str(), Box::new(expr))
                }

                _ => Stmt::ExprStmt(Box::new(parse_expr(tokens, pos))),
            },
            _ => Stmt::ExprStmt(Box::new(parse_expr(tokens, pos))),
        }
    }

    pub fn parse_program(tokens: &[Token], pos: &mut usize) -> Stmt {
        let mut stmts = Vec::new();

        while *pos < tokens.len() && tokens[*pos].token_type != TokenType::EOF {
            stmts.push(parse_stmt(tokens, pos));
        }

        Stmt::Block(stmts)
    }

    parse_program(tokens, &mut pos)
}
