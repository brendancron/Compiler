use rust_comp::components::{executor, formatter, interpreter, lexer, metaprocessor, parser};
use rust_comp::models::environment::Env;
use std::fs::File;
use std::io::{self, Read, Write};

fn main() {
    let mut out1 = io::stdout();
    let mut out2 = io::stdout();

    let mut source_file = File::create("../out/source_code.cx").unwrap();
    let mut tok_file = File::create("../out/tokens.txt").unwrap();
    let mut ast_file = File::create("../out/parsed_ast.txt").unwrap();
    let mut lowered_file = File::create("../out/lowered_ast.txt").unwrap();
    let mut full_lowered_file = File::create("../out/lowered_code.cx").unwrap();

    let mut exec = executor::Executor::new()
        .tap(move |source| {
            writeln!(source_file, "{source}").unwrap();
        })
        .then(|source: String| lexer::tokenize(&source))
        .tap(move |tokens| {
            writeln!(tok_file, "{tokens:#?}").unwrap();
        })
        .then(|tokens| parser::parse(&tokens))
        .tap(move |parsed| {
            writeln!(ast_file, "{parsed:#?}").unwrap();
        })
        .then(move |parsed| {
            let meta_env = Env::new();
            metaprocessor::lower(&parsed, meta_env, &mut out1)
        })
        .tap(move |lowered| {
            writeln!(lowered_file, "{lowered:#?}").unwrap();
        })
        .tap(move |lowered| {
            let formatted_code = formatter::format_stmts_default(&lowered);
            writeln!(full_lowered_file, "{formatted_code}").unwrap();
        })
        .then(move |lowered| {
            let env = Env::new();
            interpreter::eval(&lowered, env, &mut None, &mut out2);
        });

    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    exec.run(buf);
}
