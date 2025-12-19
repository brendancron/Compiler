use std::io::{self, Read};

mod components {
    pub mod interpreter;
    pub mod lexer;
    pub mod metaprocessor;
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
use components::metaprocessor;
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
    let parsed_code = parser::parse(&tokens);
    println!("{:#?}", parsed_code);

    let lowered_code = metaprocessor::lower_stmt(&parsed_code);
    println!("{:#?}", lowered_code);

    let mut env = Env::new();
    interpreter::eval_stmt(&lowered_code, &mut env);
}
