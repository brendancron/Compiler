use crate::models::ast::{Expr, Stmt};
use crate::models::token::{Token, TokenType};

fn check(tokens: &[Token], pos: usize, expected: TokenType) -> bool {
    matches!(
        tokens.get(pos),
        Some(t) if t.token_type == expected
    )
}

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

fn parse_separated<T>(
    tokens: &[Token],
    pos: &mut usize,
    separator: TokenType,
    terminator: TokenType,
    mut parse_item: impl FnMut(&[Token], &mut usize) -> T,
) -> Vec<T> {
    let mut items = Vec::new();

    if check(tokens, *pos, terminator) {
        return items;
    }

    loop {
        let before = *pos;
        items.push(parse_item(tokens, pos));

        if *pos == before {
            panic!("parser made no progress in comma-separated list");
        }

        if check(tokens, *pos, separator) {
            consume(tokens, pos, separator);
        } else {
            break;
        }
    }

    items
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

                TokenType::Identifier => {
                    let name = consume_next(tokens, pos).expect_str();

                    if check(tokens, *pos, TokenType::LeftParen) {
                        consume(tokens, pos, TokenType::LeftParen);
                        let args = parse_separated(
                            tokens,
                            pos,
                            TokenType::Comma,
                            TokenType::RightParen,
                            parse_expr,
                        );
                        consume(tokens, pos, TokenType::RightParen);

                        Expr::Call {
                            callee: Box::new(Expr::Variable(name)),
                            args,
                        }
                    } else {
                        Expr::Variable(name)
                    }
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
                        left = Expr::Sub(Box::new(left), Box::new(right));
                    }

                    TokenType::EqualEqual => {
                        *pos += 1;
                        let right = parse_term(tokens, pos);
                        left = Expr::Equals(Box::new(left), Box::new(right));
                    }

                    _ => return left,
                },
                _ => return left,
            }
        }
    }

    fn parse_stmt<'a>(tokens: &'a [Token], pos: &mut usize) -> Stmt {
        fn parse_expr_stmt<'a>(tokens: &'a [Token], pos: &mut usize) -> Stmt {
            let expr = parse_expr(tokens, pos);
            consume(tokens, pos, TokenType::Semicolon);
            Stmt::ExprStmt(Box::new(expr))
        }

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
                    let else_branch = if check(tokens, *pos, TokenType::Else) {
                        consume(tokens, pos, TokenType::Else);
                        if check(tokens, *pos, TokenType::If) {
                            Some(Box::new(parse_stmt(tokens, pos)))
                        } else {
                            consume(tokens, pos, TokenType::LeftBrace);
                            let stmt = parse_stmt(tokens, pos);
                            consume(tokens, pos, TokenType::RightBrace);
                            Some(Box::new(stmt))
                        }
                    } else {
                        None
                    };

                    Stmt::If {
                        cond: Box::new(conditional),
                        body: Box::new(inner),
                        else_branch: else_branch,
                    }
                }

                TokenType::Var => {
                    consume(tokens, pos, TokenType::Var);
                    let id = consume(tokens, pos, TokenType::Identifier);
                    consume(tokens, pos, TokenType::Equal);
                    let expr = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::Semicolon);
                    Stmt::Assignment {
                        name: id.expect_str(),
                        expr: Box::new(expr),
                    }
                }

                TokenType::Func => {
                    consume(tokens, pos, TokenType::Func);
                    let name = consume(tokens, pos, TokenType::Identifier).expect_str();
                    consume(tokens, pos, TokenType::LeftParen);
                    let params = parse_separated(
                        tokens,
                        pos,
                        TokenType::Comma,
                        TokenType::RightParen,
                        |tokens, pos| consume(tokens, pos, TokenType::Identifier).expect_str(),
                    );
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::LeftBrace);
                    let body = parse_block(tokens, pos);
                    consume(tokens, pos, TokenType::RightBrace);

                    Stmt::FnDecl {
                        name,
                        params,
                        body: Box::new(body),
                    }
                }

                TokenType::Return => {
                    consume(tokens, pos, TokenType::Return);
                    let opt_expr = if check(tokens, *pos, TokenType::Semicolon) {
                        None
                    } else {
                        Some(Box::new(parse_expr(tokens, pos)))
                    };
                    consume(tokens, pos, TokenType::Semicolon);
                    Stmt::Return(opt_expr)
                }

                _ => parse_expr_stmt(tokens, pos),
            },
            _ => parse_expr_stmt(tokens, pos),
        }
    }

    pub fn parse_block(tokens: &[Token], pos: &mut usize) -> Stmt {
        let mut stmts = Vec::new();

        while *pos < tokens.len() && tokens[*pos].token_type != TokenType::RightBrace {
            stmts.push(parse_stmt(tokens, pos));
        }

        Stmt::Block(stmts)
    }

    fn parse_program(tokens: &[Token], pos: &mut usize) -> Stmt {
        let mut stmts = Vec::new();

        while *pos < tokens.len() && tokens[*pos].token_type != TokenType::EOF {
            stmts.push(parse_stmt(tokens, pos));
        }

        Stmt::Block(stmts)
    }

    parse_program(tokens, &mut pos)
}
