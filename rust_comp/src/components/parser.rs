use crate::models::ast::Expr;
use crate::models::token::Token;

fn peek<'a>(tokens: &'a [Token], pos: usize) -> Option<&'a Token> {
    tokens.get(pos)
}

fn consume<'a>(tokens: &'a [Token], pos: &mut usize, expected: Token) -> Option<&'a Token> {
    match tokens.get(*pos) {
        Some(t) if *t == expected => consume_next(tokens, pos),
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

    // factor := NUMBER | STRING | TRUE | FALSE | "(" expr ")"
    fn parse_factor<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
        match tokens.get(*pos).unwrap_or(&Token::EOF) {
            Token::Number(n) => {
                consume_next(tokens, pos);
                Expr::Int(*n)
            }

            Token::String(s) => {
                consume_next(tokens, pos);
                Expr::String(s.clone())
            }

            Token::True => {
                consume_next(tokens, pos);
                Expr::Bool(true)
            }

            Token::False => {
                consume_next(tokens, pos);
                Expr::Bool(false)
            }

            Token::LeftParen => {
                consume(tokens, pos, Token::LeftParen);
                let expr = parse_expr(tokens, pos);
                consume(tokens, pos, Token::RightParen);
                expr
            }

            _ => panic!("expected literal or '('"),
        }
    }

    // term := factor (("*" factor) | ("/" factor))*
    fn parse_term<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
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
    fn parse_expr<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
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

    fn parse_keyword<'a>(tokens: &'a [Token], pos: &mut usize) -> Expr {
        match tokens.get(*pos) {
            Some(Token::Print) => {
                consume(tokens, pos, Token::Print);
                consume(tokens, pos, Token::LeftParen);
                let expr = parse_keyword(tokens, pos);
                consume(tokens, pos, Token::RightParen);
                consume(tokens, pos, Token::Semicolon);
                Expr::Print(Box::new(expr))
            }
            Some(Token::If) => {
                consume(tokens, pos, Token::If);
                consume(tokens, pos, Token::LeftParen);
                let conditional = parse_expr(tokens, pos);
                consume(tokens, pos, Token::RightParen);
                consume(tokens, pos, Token::LeftBrace);
                let inner = parse_keyword(tokens, pos);
                consume(tokens, pos, Token::RightBrace);
                Expr::If(Box::new(conditional), Box::new(inner))
            }
            Some(Token::Var) => {
                consume(tokens, pos, Token::Var);
                consume(tokens, pos, Token::Identifier);
                consume(tokens, pos, Token::Equal);
                let expr = parse_keyword(tokens, pos);
                consume(tokens, pos, Token::Semicolon);
                Expr::Assignment(, Box::new(expr))
            }
            _ => parse_expr(tokens, pos),
        }
    }

    parse_keyword(tokens, &mut pos)
}
