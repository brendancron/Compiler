#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Else,
    False,
    Func,
    For,
    If,
    Or,
    Print,
    Return,
    True,
    Var,
    While,

    // End of file
    EOF,
}

pub enum TokenMetadata {
    Int(i64),
    String(String),
}

struct Token {
    token_type: TokenType,
    line_number: usize,
    metadata: Option<TokenMetadata>,
}
