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

use components::{executor, formatter, interpreter, lexer, metaprocessor, parser};
use models::ast::{BlueprintStmt, ExpandedStmt};
use models::decl_registry::{DeclRegistry, DeclRegistryRef};
use models::environment::Env;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;

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

pub fn debug_executor<M, E>(
    meta_out: M,
    mut eval_out: E,
    out_dir: PathBuf,
) -> executor::Executor<String, ()>
where
    M: Write + 'static,
    E: Write + 'static,
{
    fs::create_dir_all(&out_dir).unwrap();
    let mut source_file = File::create(out_dir.join("source_code.cx")).unwrap();
    let mut tok_file = File::create(out_dir.join("../out/tokens.txt")).unwrap();
    let mut ast_file = File::create(out_dir.join("../out/parsed_ast.txt")).unwrap();
    let mut expanded_file = File::create(out_dir.join("../out/expanded_ast.txt")).unwrap();
    let mut full_expanded_file = File::create(out_dir.join("../out/expanded_code.cx")).unwrap();

    executor::Executor::new()
        .tap(move |source: &String| {
            writeln!(source_file, "{}", source).unwrap();
        })
        .then(|source: String| lexer::tokenize(&source))
        .tap(move |tokens: &Vec<_>| {
            writeln!(tok_file, "{:?}", tokens).unwrap();
        })
        .then(|tokens| parser::parse(&tokens))
        .tap(move |blueprint: &Vec<_>| {
            writeln!(ast_file, "{:?}", blueprint).unwrap();
        })
        .then(default_run_metaprocessor(meta_out))
        .tap(move |(expanded, _)| {
            writeln!(expanded_file, "{:?}", expanded).unwrap();
            let formatted = formatter::format_stmts_default(expanded);
            writeln!(full_expanded_file, "{:?}", formatted).unwrap();
        })
        .then(move |(expanded, decl_reg)| {
            let env = Env::new();
            interpreter::eval(&expanded, env, decl_reg, &mut None, &mut eval_out);
            ()
        })
}
