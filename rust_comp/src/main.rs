use std::io::{self, Read};

mod components {
    pub mod lexer;
    pub mod parser;
    pub mod interpreter;
}

mod models {
    pub mod token;
    pub mod ast;
    pub mod value;
}

use components::lexer;
use components::parser;
use components::interpreter;

fn main() {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    println!("\nSource Code");
    print!("{}", buf);

    let tokens = lexer::tokenize(&buf);
    println!("\nTokens");
    for tok in &tokens {
        println!("{:?}", tok);
    }

    println!("\nExpr");
    let code = parser::parse(&tokens);
    println!("{:#?}", code);
    
    interpreter::eval(&code);
}
