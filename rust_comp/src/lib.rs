mod components {
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

use components::{interpreter, lexer, metaprocessor, parser};
use models::environment::Env;
use std::io::Write;

/// Run a Cronyx program and return its stdout
pub fn run_source<M, E>(source: &str, meta_out: &mut M, eval_out: &mut E)
where
    M: Write,
    E: Write,
{
    // Lex
    let tokens = lexer::tokenize(source);

    // Parse
    let parsed_code = parser::parse(&tokens);

    // Meta-lowering
    let meta_env = Env::new();
    let lowered_code = metaprocessor::lower(&parsed_code, meta_env, meta_out);

    let env = Env::new();
    interpreter::eval(&lowered_code, env, &mut None, eval_out);
}
