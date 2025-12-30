pub mod components {
    pub mod executor;
    pub mod interpreter;
    pub mod lexer;
    pub mod metaprocessor;
    pub mod parser;
    pub mod substitution;
}

pub mod models {
    pub mod ast;
    pub mod environment;
    pub mod result;
    pub mod token;
    pub mod value;
}

use components::{executor, interpreter, lexer, metaprocessor, parser};
use models::environment::Env;
use std::io::Write;

pub fn default_executor<M, E>(mut meta_out: M, mut eval_out: E) -> executor::Executor<String, ()>
where
    M: Write + 'static,
    E: Write + 'static,
{
    executor::Executor::new()
        .then(|source: String| lexer::tokenize(&source))
        .then(|tokens| parser::parse(&tokens))
        .then(move |parsed| {
            let meta_env = Env::new();
            metaprocessor::lower(&parsed, meta_env, &mut meta_out)
        })
        .then(move |lowered| {
            let env = Env::new();
            interpreter::eval(&lowered, env, &mut None, &mut eval_out);
        })
}
