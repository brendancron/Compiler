use crate::models::token::Token;

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_alpha_numeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}

fn lex_number(chars: &[char], mut i: usize) -> (Token, usize) {
    let mut acc = String::new();

    while i < chars.len() && is_digit(chars[i]) {
        acc.push(chars[i]);
        i += 1;
    }

    (Token::Number(acc.parse::<i64>().unwrap()), i)
}

fn lex_identifier(chars: &[char], mut i: usize) -> (String, usize) {
    let mut acc = String::new();

    while i < chars.len() && is_alpha_numeric(chars[i]) {
        acc.push(chars[i]);
        i += 1;
    }

    (acc, i)
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();
    let mut tokens = Vec::new();

    let mut i = 0;
    while i < len {
        let c = chars[i];

        match c {
            ' ' | '\n' | '\t' => {
                i += 1;
            }

            '(' => {
                tokens.push(Token::LeftParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RightParen);
                i += 1;
            }

            // FIXED: correct brace handling `{` and `}`
            '{' => {
                tokens.push(Token::LeftBrace);
                i += 1;
            }
            '}' => {
                tokens.push(Token::RightBrace);
                i += 1;
            }

            ',' => {
                tokens.push(Token::Comma);
                i += 1;
            }
            '.' => {
                tokens.push(Token::Dot);
                i += 1;
            }
            '-' => {
                tokens.push(Token::Minus);
                i += 1;
            }
            '+' => {
                tokens.push(Token::Plus);
                i += 1;
            }
            ';' => {
                tokens.push(Token::Semicolon);
                i += 1;
            }
            '/' => {
                tokens.push(Token::Slash);
                i += 1;
            }
            '*' => {
                tokens.push(Token::Star);
                i += 1;
            }

            '!' => {
                if i + 1 < len && chars[i + 1] == '=' {
                    tokens.push(Token::BangEqual);
                    i += 2;
                } else {
                    tokens.push(Token::Bang);
                    i += 1;
                }
            }

            '=' => {
                if i + 1 < len && chars[i + 1] == '=' {
                    tokens.push(Token::EqualEqual);
                    i += 2;
                } else {
                    tokens.push(Token::Equal);
                    i += 1;
                }
            }

            '>' => {
                if i + 1 < len && chars[i + 1] == '=' {
                    tokens.push(Token::GreaterEqual);
                    i += 2;
                } else {
                    tokens.push(Token::Greater);
                    i += 1;
                }
            }

            '<' => {
                if i + 1 < len && chars[i + 1] == '=' {
                    tokens.push(Token::LessEqual);
                    i += 2;
                } else {
                    tokens.push(Token::Less);
                    i += 1;
                }
            }

            c if is_digit(c) => {
                let (tok, j) = lex_number(&chars, i);
                tokens.push(tok);
                i = j;
            }

            c if is_alpha(c) => {
                let (name, j) = lex_identifier(&chars, i);

                let tok = match name.as_str() {
                    "and" => Token::And,
                    "else" => Token::Else,
                    "false" => Token::False,
                    "fn" => Token::Func,
                    "for" => Token::For,
                    "if" => Token::If,
                    "or" => Token::Or,
                    "print" => Token::Print,
                    "return" => Token::Return,
                    "true" => Token::True,
                    "var" => Token::Var,
                    "while" => Token::While,
                    _ => Token::Identifier(name),
                };

                tokens.push(tok);
                i = j;
            }

            '"' => {
                let mut acc = String::new();
                let mut j = i + 1;

                while j < len {
                    match chars[j] {
                        '"' => {
                            tokens.push(Token::String(acc));
                            i = j + 1;
                            break;
                        }
                        c => {
                            acc.push(c);
                            j += 1;
                        }
                    }
                }

                if j >= len {
                    panic!("unterminated string");
                }
            }

            _ => panic!("unexpected character: {}", c),
        }
    }

    tokens.push(Token::EOF);
    tokens
}
