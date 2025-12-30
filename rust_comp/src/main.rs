use std::fs::File;
use std::io::{self, Read, Write};

mod components {
    pub mod formatter;
    pub mod interpreter;
    pub mod lexer;
    pub mod metaprocessor;
    pub mod parser;
    pub mod substitution;
}

mod models {
    pub mod ast;
    pub mod environment;
    pub mod result;
    pub mod token;
    pub mod value;
}

use components::formatter;
use components::interpreter;
use components::lexer;
use components::metaprocessor;
use components::parser;

use models::environment::Env;

fn main() {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();

    let mut file = File::create("../out/source_code.txt").expect("failed to create output file");
    writeln!(file, "{:#?}", buf).expect("failed to write parsed AST");

    let tokens = lexer::tokenize(&buf);

    let mut file = File::create("../out/tokens.txt").expect("failed to create tokens file");

    for tok in &tokens {
        writeln!(file, "{:?}", tok).expect("failed to write token");
    }

    let parsed_code = parser::parse(&tokens);

    let mut file = File::create("../out/parsed_ast.txt").expect("failed to create output file");
    writeln!(file, "{:#?}", parsed_code).expect("failed to write parsed AST");

    let lowered_code = metaprocessor::lower(&parsed_code);
    let mut file = File::create("../out/lowered_ast.txt").expect("failed to create output file");
    writeln!(file, "{:#?}", lowered_code).expect("failed to write lowered AST");

    let formatted_code = formatter::format_stmts_default(&lowered_code);
    let mut file = File::create("../out/lowered_code.cx").expect("failed to create lowered_code.cx");
    write!(file, "{}", formatted_code).expect("failed to write formatted code");

    let env = Env::new();
    interpreter::eval(&lowered_code, env, &mut None);
}
