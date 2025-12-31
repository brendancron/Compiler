pub mod components {
    pub mod executor;
    pub mod formatter;
    pub mod interpreter;
    pub mod lexer;
    pub mod metaprocessor;
    pub mod parser;
    pub mod substitution;
}

pub mod models {
    pub mod ast;
    pub mod decl_registry;
    pub mod environment;
    pub mod result;
    pub mod token;
    pub mod value;
}

use components::{executor, interpreter, lexer, metaprocessor, parser};
use models::ast::{LoweredStmt, ParsedStmt};
use models::decl_registry::{DeclRegistry, DeclRegistryRef};
use models::environment::Env;
use std::io::Write;

pub fn default_run_metaprocessor<W: Write + 'static>(
    mut out: W,
) -> impl FnMut(Vec<ParsedStmt>) -> (Vec<LoweredStmt>, DeclRegistryRef) {
    move |parsed| {
        let meta_env = Env::new();
        let decl_reg = DeclRegistry::new();
        let lowered = metaprocessor::lower(&parsed, meta_env, decl_reg.clone(), &mut out);
        (lowered, decl_reg)
    }
}

pub fn default_executor<M, E>(meta_out: M, mut eval_out: E) -> executor::Executor<String, ()>
where
    M: Write + 'static,
    E: Write + 'static,
{
    executor::Executor::new()
        .then(|source: String| lexer::tokenize(&source))
        .then(|tokens| parser::parse(&tokens))
        .then(default_run_metaprocessor(meta_out))
        .then(move |(lowered, decl_reg)| {
            let env = Env::new();
            interpreter::eval(&lowered, env, decl_reg, &mut None, &mut eval_out);
        })
}
