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
    pub mod types;
    pub mod value;
}

use components::{executor, interpreter, lexer, metaprocessor, parser};
use models::ast::{BlueprintStmt, ExpandedStmt};
use models::decl_registry::{DeclRegistry, DeclRegistryRef};
use models::environment::Env;
use std::io::Write;

pub type CompilerError = String;

pub fn default_run_metaprocessor<W: Write + 'static>(
    mut out: W,
) -> impl FnMut(Vec<BlueprintStmt>) -> (Vec<ExpandedStmt>, DeclRegistryRef) {
    move |parsed| {
        let meta_env = Env::new();
        let decl_reg = DeclRegistry::new();
        let processed = metaprocessor::process(&parsed, meta_env, decl_reg.clone(), &mut out);
        (processed, decl_reg)
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
        .then(move |(expanded, decl_reg)| {
            let env = Env::new();
            interpreter::eval(&expanded, env, decl_reg, &mut None, &mut eval_out);
            ()
        })
}
