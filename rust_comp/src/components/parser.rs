use crate::models::ast::{BlueprintExpr, BlueprintFuncType, BlueprintStmt, TypeExpr};
use crate::models::token::{Token, TokenType};

fn peek(tokens: &[Token], pos: usize) -> Option<TokenType> {
    match tokens.get(pos) {
        None => None,
        Some(t) => Some(t.token_type),
    }
}

fn check(tokens: &[Token], pos: usize, expected: TokenType) -> bool {
    match peek(tokens, pos) {
        None => false,
        Some(token_type) => token_type == expected,
    }
}

fn consume<'a>(tokens: &'a [Token], pos: &mut usize, expected: TokenType) -> &'a Token {
    match tokens.get(*pos) {
        Some(t) if t.token_type == expected => consume_next(tokens, pos),
        Some(t) => panic!(
            "expected {:?}, found {:?} on line {} at position {}",
            expected, t.token_type, t.line_number, pos
        ),
        None => panic!("expected {:?}, found EOF at position {}", expected, pos),
    }
}

fn consume_next<'a>(tokens: &'a [Token], pos: &mut usize) -> &'a Token {
    let tok = tokens
        .get(*pos)
        .expect("internal error: consume_next out of bounds");
    *pos += 1;
    //println!("Consumed: {:?}", tok);
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

fn parse_type(tokens: &[Token], pos: &mut usize) -> TypeExpr {
    let name = consume(tokens, pos, TokenType::Identifier).expect_str();

    match name.as_str() {
        "int" => TypeExpr::Int,
        "string" => TypeExpr::String,
        "bool" => TypeExpr::Bool,
        _ => TypeExpr::Named(name),
    }
}

pub fn parse(tokens: &[Token]) -> Vec<BlueprintStmt> {
    let mut pos = 0;

    fn parse_factor<'a>(tokens: &'a [Token], pos: &mut usize) -> BlueprintExpr {
        match tokens.get(*pos) {
            Some(tok) => match tok.token_type {
                TokenType::Number => {
                    consume_next(tokens, pos);
                    BlueprintExpr::Int(tok.expect_int())
                }

                TokenType::String => {
                    consume_next(tokens, pos);
                    BlueprintExpr::String(tok.expect_str())
                }

                TokenType::True => {
                    consume_next(tokens, pos);
                    BlueprintExpr::Bool(true)
                }

                TokenType::False => {
                    consume_next(tokens, pos);
                    BlueprintExpr::Bool(false)
                }

                TokenType::LeftParen => {
                    consume(tokens, pos, TokenType::LeftParen);
                    let expr = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    expr
                }

                TokenType::Typeof => {
                    consume(tokens, pos, TokenType::Typeof);
                    consume(tokens, pos, TokenType::LeftParen);
                    let id = consume(tokens, pos, TokenType::Identifier).expect_str();
                    consume(tokens, pos, TokenType::RightParen);
                    BlueprintExpr::Typeof(id)
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

                        BlueprintExpr::Call { callee: name, args }
                    } else if check(tokens, *pos, TokenType::LeftBrace) {
                        consume(tokens, pos, TokenType::LeftBrace);

                        let fields = parse_separated(
                            tokens,
                            pos,
                            TokenType::Comma,
                            TokenType::RightBrace,
                            |tokens, pos| {
                                let field_name =
                                    consume(tokens, pos, TokenType::Identifier).expect_str();
                                consume(tokens, pos, TokenType::Colon);
                                let expr = parse_expr(tokens, pos);
                                (field_name, Box::new(expr))
                            },
                        );

                        consume(tokens, pos, TokenType::RightBrace);

                        BlueprintExpr::StructLiteral {
                            type_name: name,
                            fields,
                        }
                    } else {
                        BlueprintExpr::Variable(name)
                    }
                }

                TokenType::LeftBracket => {
                    consume(tokens, pos, TokenType::LeftBracket);

                    let elems = parse_separated(
                        tokens,
                        pos,
                        TokenType::Comma,
                        TokenType::RightBracket,
                        parse_expr,
                    );

                    consume(tokens, pos, TokenType::RightBracket);

                    BlueprintExpr::List(elems)
                }

                _ => panic!("expected literal or '('"),
            },
            None => panic!("Unexpected EOF"),
        }
    }

    fn parse_term<'a>(tokens: &'a [Token], pos: &mut usize) -> BlueprintExpr {
        let mut left = parse_factor(tokens, pos);

        loop {
            match tokens.get(*pos) {
                Some(tok) => match tok.token_type {
                    TokenType::Star => {
                        *pos += 1;
                        let right = parse_factor(tokens, pos);
                        left = BlueprintExpr::Mult(Box::new(left), Box::new(right));
                    }
                    TokenType::Slash => {
                        *pos += 1;
                        let right = parse_factor(tokens, pos);
                        left = BlueprintExpr::Div(Box::new(left), Box::new(right));
                    }
                    _ => return left,
                },
                _ => return left,
            }
        }
    }

    fn parse_expr<'a>(tokens: &'a [Token], pos: &mut usize) -> BlueprintExpr {
        let mut left = parse_term(tokens, pos);

        loop {
            match tokens.get(*pos) {
                Some(tok) => match tok.token_type {
                    TokenType::Plus => {
                        *pos += 1;
                        let right = parse_term(tokens, pos);
                        left = BlueprintExpr::Add(Box::new(left), Box::new(right));
                    }

                    TokenType::Minus => {
                        *pos += 1;
                        let right = parse_term(tokens, pos);
                        left = BlueprintExpr::Sub(Box::new(left), Box::new(right));
                    }

                    TokenType::EqualEqual => {
                        *pos += 1;
                        let right = parse_term(tokens, pos);
                        left = BlueprintExpr::Equals(Box::new(left), Box::new(right));
                    }

                    _ => return left,
                },
                _ => return left,
            }
        }
    }

    fn parse_stmt<'a>(tokens: &'a [Token], pos: &mut usize) -> BlueprintStmt {
        fn parse_expr_stmt<'a>(tokens: &'a [Token], pos: &mut usize) -> BlueprintStmt {
            let expr = parse_expr(tokens, pos);
            consume(tokens, pos, TokenType::Semicolon);
            BlueprintStmt::ExprStmt(Box::new(expr))
        }

        match tokens.get(*pos) {
            Some(tok) => match tok.token_type {
                TokenType::Print => {
                    consume(tokens, pos, TokenType::Print);
                    consume(tokens, pos, TokenType::LeftParen);
                    let expr = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::Semicolon);
                    BlueprintStmt::Print(Box::new(expr))
                }

                TokenType::If => {
                    consume(tokens, pos, TokenType::If);
                    consume(tokens, pos, TokenType::LeftParen);
                    let conditional = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::LeftBrace);
                    let inner = parse_block(tokens, pos);
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

                    BlueprintStmt::If {
                        cond: Box::new(conditional),
                        body: Box::new(inner),
                        else_branch: else_branch,
                    }
                }

                TokenType::For => {
                    consume(tokens, pos, TokenType::For);
                    consume(tokens, pos, TokenType::LeftParen);
                    let name = consume(tokens, pos, TokenType::Identifier).expect_str();
                    consume(tokens, pos, TokenType::In);
                    let iter = Box::new(parse_expr(tokens, pos));
                    consume(tokens, pos, TokenType::RightParen);
                    consume(tokens, pos, TokenType::LeftBrace);
                    let inner = Box::new(parse_block(tokens, pos));
                    consume(tokens, pos, TokenType::RightBrace);
                    BlueprintStmt::ForEach {
                        var: name,
                        iterable: iter,
                        body: inner,
                    }
                }

                TokenType::Var => {
                    consume(tokens, pos, TokenType::Var);
                    let id = consume(tokens, pos, TokenType::Identifier);
                    consume(tokens, pos, TokenType::Equal);
                    let expr = parse_expr(tokens, pos);
                    consume(tokens, pos, TokenType::Semicolon);
                    BlueprintStmt::Assignment {
                        name: id.expect_str(),
                        expr: Box::new(expr),
                    }
                }

                TokenType::Func => parse_fn_decl(tokens, pos, BlueprintFuncType::Normal),

                TokenType::Struct => {
                    consume(tokens, pos, TokenType::Struct);
                    let name = consume(tokens, pos, TokenType::Identifier).expect_str();
                    consume(tokens, pos, TokenType::LeftBrace);
                    let fields = parse_separated(
                        tokens,
                        pos,
                        TokenType::Semicolon,
                        TokenType::RightBrace,
                        |tokens, pos| {
                            let field_name =
                                consume(tokens, pos, TokenType::Identifier).expect_str();
                            consume(tokens, pos, TokenType::Colon);
                            let ty = parse_type(tokens, pos);
                            (field_name, ty)
                        },
                    );

                    consume(tokens, pos, TokenType::RightBrace);

                    BlueprintStmt::StructDecl { name, fields }
                }

                TokenType::Return => {
                    consume(tokens, pos, TokenType::Return);
                    let opt_expr = if check(tokens, *pos, TokenType::Semicolon) {
                        None
                    } else {
                        Some(Box::new(parse_expr(tokens, pos)))
                    };
                    consume(tokens, pos, TokenType::Semicolon);
                    BlueprintStmt::Return(opt_expr)
                }

                TokenType::Gen => {
                    consume(tokens, pos, TokenType::Gen);
                    let stmt = parse_stmt(tokens, pos);
                    BlueprintStmt::Gen(vec![stmt])
                }

                TokenType::LeftBrace => {
                    consume(tokens, pos, TokenType::LeftBrace);
                    let body = parse_block(tokens, pos);
                    consume(tokens, pos, TokenType::RightBrace);
                    body
                }

                TokenType::Meta => parse_meta_stmt(tokens, pos),

                _ => parse_expr_stmt(tokens, pos),
            },
            _ => parse_expr_stmt(tokens, pos),
        }
    }

    fn parse_meta_stmt(tokens: &[Token], pos: &mut usize) -> BlueprintStmt {
        consume(tokens, pos, TokenType::Meta);

        match peek(tokens, *pos) {
            Some(TokenType::Func) => parse_fn_decl(tokens, pos, BlueprintFuncType::Meta),
            _ => {
                let stmt = parse_stmt(tokens, pos);
                BlueprintStmt::MetaStmt(Box::new(stmt))
            }
        }
    }

    fn parse_fn_decl(
        tokens: &[Token],
        pos: &mut usize,
        func_type: BlueprintFuncType,
    ) -> BlueprintStmt {
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

        BlueprintStmt::FnDecl {
            name,
            func_type,
            params,
            body: Box::new(body),
        }
    }

    fn parse_block(tokens: &[Token], pos: &mut usize) -> BlueprintStmt {
        let mut stmts = Vec::new();

        while *pos < tokens.len() && tokens[*pos].token_type != TokenType::RightBrace {
            stmts.push(parse_stmt(tokens, pos));
        }

        BlueprintStmt::Block(stmts)
    }

    fn parse_program(tokens: &[Token], pos: &mut usize) -> Vec<BlueprintStmt> {
        let mut stmts = Vec::new();

        while *pos < tokens.len() && tokens[*pos].token_type != TokenType::EOF {
            stmts.push(parse_stmt(tokens, pos));
        }

        stmts
    }

    parse_program(tokens, &mut pos)
}
