use std::io::{self, Read};

mod components {
    pub mod interpreter;
    pub mod lexer;
    pub mod parser;
}

mod models {
    pub mod ast;
    pub mod environment;
    pub mod result;
    pub mod token;
    pub mod value;
}

use components::interpreter;
use components::lexer;
use components::parser;

use models::environment::Env;

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

    let mut env = Env::new();
    interpreter::eval_stmt(&code, &mut env);
}
