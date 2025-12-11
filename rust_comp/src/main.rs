use std::io::{self, Read};

mod components {
    pub mod lexer;
    pub mod parser;
}

mod models {
    pub mod token;
    pub mod ast;
}

use components::lexer;
use components::parser;

fn main() {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    print!("{}", buf);
    let tokens = lexer::tokenize(&buf);
    for tok in &tokens {
        println!("{:?}", tok);
    }

    let expr = parser::parse(&tokens);

    println!("{:#?}", expr);
}
