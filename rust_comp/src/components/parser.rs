use crate::models::semantics::blueprint_ast::{
    BlueprintExpr, BlueprintFuncType, BlueprintStmt, TypeExpr, BlueprintAst,
};
use crate::models::token::{Token, TokenType};

pub struct ParseCtx {
    pub imports: Vec<String>,
}

impl ParseCtx {
    pub fn new() -> Self {
        Self {
            imports: vec![],
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnterminatedString,
    UnexpectedToken {
        found: TokenType,
        expected: TokenType,
        pos: usize,
    },
    UnexpectedEOF {
        expected: TokenType,
        pos: usize,
    },
}

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

fn consume<'a>(
    tokens: &'a [Token],
    pos: &mut usize,
    expected: TokenType,
) -> Result<&'a Token, ParseError> {
    match tokens.get(*pos) {
        Some(t) if t.token_type == expected => Ok(consume_next(tokens, pos)),
        Some(t) => Err(ParseError::UnexpectedToken {
            found: t.token_type,
            expected,
            pos: *pos,
        }),
        None => Err(ParseError::UnexpectedEOF {
            expected,
            pos: *pos,
        }),
    }
}

fn consume_next<'a>(tokens: &'a [Token], pos: &mut usize) -> &'a Token {
    let tok = tokens
        .get(*pos)
        .expect("internal error: consume_next out of bounds");
    *pos += 1;
    tok
}

fn parse_separated<T>(
    tokens: &[Token],
    pos: &mut usize,
    ctx: &mut ParseCtx,
    separator: TokenType,
    terminator: TokenType,
    mut parse_item: impl FnMut(&[Token], &mut usize, &mut ParseCtx) -> Result<T, ParseError>,
) -> Result<Vec<T>, ParseError> {
    let mut items = Vec::new();

    if check(tokens, *pos, terminator) {
        return Ok(items);
    }

    loop {
        let before = *pos;
        items.push(parse_item(tokens, pos, ctx)?);

        if *pos == before {
            panic!("parser made no progress in comma-separated list");
        }

        if check(tokens, *pos, separator) {
            consume(tokens, pos, separator)?;
        } else {
            break;
        }
    }

    Ok(items)
}

fn parse_type(tokens: &[Token], pos: &mut usize) -> Result<TypeExpr, ParseError> {
    let name = consume(tokens, pos, TokenType::Identifier)?.expect_str();

    match name.as_str() {
        "int" => Ok(TypeExpr::Int),
        "string" => Ok(TypeExpr::String),
        "bool" => Ok(TypeExpr::Bool),
        _ => Ok(TypeExpr::Named(name)),
    }
}


fn parse_factor<'a>(tokens: &'a [Token], pos: &mut usize, ctx: &mut ParseCtx) -> Result<BlueprintExpr, ParseError> {
    match tokens.get(*pos) {
        Some(tok) => match tok.token_type {
            TokenType::Number => {
                consume_next(tokens, pos);
                Ok(BlueprintExpr::Int(tok.expect_int()))
            }

            TokenType::String => {
                consume_next(tokens, pos);
                Ok(BlueprintExpr::String(tok.expect_str()))
            }

            TokenType::True => {
                consume_next(tokens, pos);
                Ok(BlueprintExpr::Bool(true))
            }

            TokenType::False => {
                consume_next(tokens, pos);
                Ok(BlueprintExpr::Bool(false))
            }

            TokenType::LeftParen => {
                consume(tokens, pos, TokenType::LeftParen)?;
                let expr = parse_expr(tokens, pos, ctx)?;
                consume(tokens, pos, TokenType::RightParen)?;
                Ok(expr)
            }

            TokenType::Typeof => {
                consume(tokens, pos, TokenType::Typeof)?;
                consume(tokens, pos, TokenType::LeftParen)?;
                let id = consume(tokens, pos, TokenType::Identifier)?.expect_str();
                consume(tokens, pos, TokenType::RightParen)?;
                Ok(BlueprintExpr::Typeof(id))
            }

            TokenType::Embed => {
                consume(tokens, pos, TokenType::Embed)?;
                consume(tokens, pos, TokenType::LeftParen)?;
                let file_path = consume(tokens, pos, TokenType::String)?.expect_str();
                consume(tokens, pos, TokenType::RightParen)?;
                Ok(BlueprintExpr::Embed(file_path))
            }

            TokenType::Import => {
                consume(tokens, pos, TokenType::Import)?;
                consume(tokens, pos, TokenType::LeftParen)?;
                let mod_name = consume(tokens, pos, TokenType::String)?.expect_str();
                consume(tokens, pos, TokenType::RightParen)?;
                Ok(BlueprintExpr::Import(mod_name))
            }


            TokenType::Identifier => {
                let name = consume_next(tokens, pos).expect_str();

                if check(tokens, *pos, TokenType::LeftParen) {
                    consume(tokens, pos, TokenType::LeftParen)?;
                    let args = parse_separated(
                        tokens,
                        pos,
                        ctx,
                        TokenType::Comma,
                        TokenType::RightParen,
                        parse_expr,
                    )?;
                    consume(tokens, pos, TokenType::RightParen)?;

                    Ok(BlueprintExpr::Call { callee: name, args })
                } else if check(tokens, *pos, TokenType::LeftBrace) {
                    consume(tokens, pos, TokenType::LeftBrace)?;

                    let fields = parse_separated(
                        tokens,
                        pos,
                        ctx,
                        TokenType::Comma,
                        TokenType::RightBrace,
                        |tokens, pos, ctx| {
                            let field_name =
                                consume(tokens, pos, TokenType::Identifier)?.expect_str();
                            consume(tokens, pos, TokenType::Colon)?;
                            let expr = parse_expr(tokens, pos, ctx)?;
                            Ok((field_name, Box::new(expr)))
                        },
                    )?;

                    consume(tokens, pos, TokenType::RightBrace)?;

                    Ok(BlueprintExpr::StructLiteral {
                        type_name: name,
                        fields,
                    })
                } else {
                    Ok(BlueprintExpr::Variable(name))
                }
            }

            TokenType::LeftBracket => {
                consume(tokens, pos, TokenType::LeftBracket)?;

                let elems = parse_separated(
                    tokens,
                    pos,
                    ctx,
                    TokenType::Comma,
                    TokenType::RightBracket,
                    parse_expr,
                )?;

                consume(tokens, pos, TokenType::RightBracket)?;

                Ok(BlueprintExpr::List(elems))
            }

            _ => panic!("expected literal or '('"),
        },
        None => panic!("Unexpected EOF"),
    }
}

fn parse_term<'a>(tokens: &'a [Token], pos: &mut usize, ctx: &mut ParseCtx) -> Result<BlueprintExpr, ParseError> {
    let mut left = parse_factor(tokens, pos, ctx)?;

    loop {
        match tokens.get(*pos) {
            Some(tok) => match tok.token_type {
                TokenType::Star => {
                    *pos += 1;
                    let right = parse_factor(tokens, pos, ctx)?;
                    left = BlueprintExpr::Mult(Box::new(left), Box::new(right));
                }
                TokenType::Slash => {
                    *pos += 1;
                    let right = parse_factor(tokens, pos, ctx)?;
                    left = BlueprintExpr::Div(Box::new(left), Box::new(right));
                }
                _ => return Ok(left),
            },
            _ => return Ok(left),
        }
    }
}


fn parse_expr<'a>(tokens: &'a [Token], pos: &mut usize, ctx: &mut ParseCtx) -> Result<BlueprintExpr, ParseError> {
    let mut left = parse_term(tokens, pos, ctx)?;

    loop {
        match tokens.get(*pos) {
            Some(tok) => match tok.token_type {
                TokenType::Plus => {
                    *pos += 1;
                    let right = parse_term(tokens, pos, ctx)?;
                    left = BlueprintExpr::Add(Box::new(left), Box::new(right));
                }

                TokenType::Minus => {
                    *pos += 1;
                    let right = parse_term(tokens, pos, ctx)?;
                    left = BlueprintExpr::Sub(Box::new(left), Box::new(right));
                }

                TokenType::EqualEqual => {
                    *pos += 1;
                    let right = parse_term(tokens, pos, ctx)?;
                    left = BlueprintExpr::Equals(Box::new(left), Box::new(right));
                }

                _ => return Ok(left),
            },
            _ => return Ok(left),
        }
    }
}

fn parse_expr_stmt<'a>(
    tokens: &'a [Token],
    pos: &mut usize,
    ctx: &mut ParseCtx,
) -> Result<BlueprintStmt, ParseError> {
    let expr = parse_expr(tokens, pos, ctx)?;
    consume(tokens, pos, TokenType::Semicolon)?;
    Ok(BlueprintStmt::ExprStmt(Box::new(expr)))
}

fn parse_stmt<'a>(tokens: &'a [Token], pos: &mut usize, ctx: &mut ParseCtx) -> Result<BlueprintStmt, ParseError> {

    match tokens.get(*pos) {
        Some(tok) => match tok.token_type {
            TokenType::Print => {
                consume(tokens, pos, TokenType::Print)?;
                consume(tokens, pos, TokenType::LeftParen)?;
                let expr = parse_expr(tokens, pos, ctx)?;
                consume(tokens, pos, TokenType::RightParen)?;
                consume(tokens, pos, TokenType::Semicolon)?;
                Ok(BlueprintStmt::Print(Box::new(expr)))
            }

            TokenType::If => {
                consume(tokens, pos, TokenType::If)?;
                consume(tokens, pos, TokenType::LeftParen)?;
                let conditional = parse_expr(tokens, pos, ctx)?;
                consume(tokens, pos, TokenType::RightParen)?;
                consume(tokens, pos, TokenType::LeftBrace)?;
                let inner = parse_block(tokens, pos, ctx)?;
                consume(tokens, pos, TokenType::RightBrace)?;
                let else_branch = if check(tokens, *pos, TokenType::Else) {
                    consume(tokens, pos, TokenType::Else)?;
                    if check(tokens, *pos, TokenType::If) {
                        Some(Box::new(parse_stmt(tokens, pos, ctx)?))
                    } else {
                        consume(tokens, pos, TokenType::LeftBrace)?;
                        let stmt = parse_stmt(tokens, pos, ctx)?;
                        consume(tokens, pos, TokenType::RightBrace)?;
                        Some(Box::new(stmt))
                    }
                } else {
                    None
                };

                Ok(BlueprintStmt::If {
                    cond: Box::new(conditional),
                    body: Box::new(inner),
                    else_branch: else_branch,
                })
            }

            TokenType::For => {
                consume(tokens, pos, TokenType::For)?;
                consume(tokens, pos, TokenType::LeftParen)?;
                let name = consume(tokens, pos, TokenType::Identifier)?.expect_str();
                consume(tokens, pos, TokenType::In)?;
                let iter = Box::new(parse_expr(tokens, pos, ctx)?);
                consume(tokens, pos, TokenType::RightParen)?;
                consume(tokens, pos, TokenType::LeftBrace)?;
                let inner = Box::new(parse_block(tokens, pos, ctx)?);
                consume(tokens, pos, TokenType::RightBrace)?;
                Ok(BlueprintStmt::ForEach {
                    var: name,
                    iterable: iter,
                    body: inner,
                })
            }

            TokenType::Var => {
                consume(tokens, pos, TokenType::Var)?;
                let id = consume(tokens, pos, TokenType::Identifier)?;
                consume(tokens, pos, TokenType::Equal)?;
                let expr = parse_expr(tokens, pos, ctx)?;
                consume(tokens, pos, TokenType::Semicolon)?;
                Ok(BlueprintStmt::Assignment {
                    name: id.expect_str(),
                    expr: Box::new(expr),
                })
            }

            TokenType::Func => parse_fn_decl(tokens, pos, ctx, BlueprintFuncType::Normal),

            TokenType::Struct => {
                consume(tokens, pos, TokenType::Struct)?;
                let name = consume(tokens, pos, TokenType::Identifier)?.expect_str();
                consume(tokens, pos, TokenType::LeftBrace)?;
                let fields = parse_separated(
                    tokens,
                    pos, 
                    ctx,
                    TokenType::Semicolon,
                    TokenType::RightBrace,
                    |tokens, pos, ctx| {
                        let field_name =
                            consume(tokens, pos, TokenType::Identifier)?.expect_str();
                        consume(tokens, pos, TokenType::Colon)?;
                        let ty = parse_type(tokens, pos)?;
                        Ok((field_name, ty))
                    },
                )?;

                consume(tokens, pos, TokenType::RightBrace)?;

                Ok(BlueprintStmt::StructDecl { name, fields })
            }

            TokenType::Return => {
                consume(tokens, pos, TokenType::Return)?;
                let opt_expr = if check(tokens, *pos, TokenType::Semicolon) {
                    None
                } else {
                    Some(Box::new(parse_expr(tokens, pos, ctx)?))
                };
                consume(tokens, pos, TokenType::Semicolon)?;
                Ok(BlueprintStmt::Return(opt_expr))
            }

            TokenType::Gen => {
                consume(tokens, pos, TokenType::Gen)?;
                let stmt = parse_stmt(tokens, pos, ctx)?;
                Ok(BlueprintStmt::Gen(vec![stmt]))
            }

            TokenType::LeftBrace => {
                consume(tokens, pos, TokenType::LeftBrace)?;
                let body = parse_block(tokens, pos, ctx)?;
                consume(tokens, pos, TokenType::RightBrace)?;
                Ok(body)
            }

            TokenType::Meta => parse_meta_stmt(tokens, pos, ctx),

            _ => parse_expr_stmt(tokens, pos, ctx),
        },
        _ => parse_expr_stmt(tokens, pos, ctx),
    }
}

fn parse_meta_stmt(tokens: &[Token], pos: &mut usize, ctx: &mut ParseCtx) -> Result<BlueprintStmt, ParseError> {
    consume(tokens, pos, TokenType::Meta)?;

    match peek(tokens, *pos) {
        Some(TokenType::Func) => parse_fn_decl(tokens, pos, ctx, BlueprintFuncType::Meta),
        _ => {
            let stmt = parse_stmt(tokens, pos, ctx)?;
            Ok(BlueprintStmt::MetaStmt(Box::new(stmt)))
        }
    }
}

fn parse_fn_decl(
    tokens: &[Token],
    pos: &mut usize,
    ctx: &mut ParseCtx,
    func_type: BlueprintFuncType,
) -> Result<BlueprintStmt, ParseError> {
    consume(tokens, pos, TokenType::Func)?;
    let name = consume(tokens, pos, TokenType::Identifier)?.expect_str();

    consume(tokens, pos, TokenType::LeftParen)?;
    let params = parse_separated(
        tokens,
        pos,
        ctx,
        TokenType::Comma,
        TokenType::RightParen,
        |tokens, pos, ctx| Ok(consume(tokens, pos, TokenType::Identifier)?.expect_str()),
    )?;
    consume(tokens, pos, TokenType::RightParen)?;

    consume(tokens, pos, TokenType::LeftBrace)?;
    let body = parse_block(tokens, pos, ctx)?;
    consume(tokens, pos, TokenType::RightBrace)?;

    Ok(BlueprintStmt::FnDecl {
        name,
        func_type,
        params,
        body: Box::new(body),
    })
}

fn parse_block(tokens: &[Token], pos: &mut usize, ctx: &mut ParseCtx) -> Result<BlueprintStmt, ParseError> {
    let mut stmts = Vec::new();

    while *pos < tokens.len() && tokens[*pos].token_type != TokenType::RightBrace {
        stmts.push(parse_stmt(tokens, pos, ctx)?);
    }

    Ok(BlueprintStmt::Block(stmts))
}

pub fn parse(tokens: &[Token], ctx: &mut ParseCtx) -> Result<BlueprintAst, ParseError> {
    let mut pos: usize = 0;

    let mut stmts = Vec::new();

    while pos < tokens.len() && tokens[pos].token_type != TokenType::EOF {
        stmts.push(parse_stmt(tokens, &mut pos, ctx)?);
    }

    Ok(BlueprintAst{stmts})
}
