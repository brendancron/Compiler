use rust_comp::components::{executor, formatter, interpreter, lexer, parser};
use rust_comp::models::environment::Env;
use std::env;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::PathBuf;

fn main() {
    let out1 = io::stdout();
    let mut out2 = io::stdout();

    let out_dir = env::args()
        .skip_while(|a| a != "--out")
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("out"));

    fs::create_dir_all(&out_dir).unwrap();
    let mut source_file = File::create(out_dir.join("source_code.cx")).unwrap();
    let mut tok_file = File::create(out_dir.join("../out/tokens.txt")).unwrap();
    let mut ast_file = File::create(out_dir.join("../out/parsed_ast.txt")).unwrap();
    let mut expanded_file = File::create(out_dir.join("../out/expanded_ast.txt")).unwrap();
    let mut full_expanded_file = File::create(out_dir.join("../out/expanded_code.cx")).unwrap();

    let mut exec = executor::Executor::new()
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
        .then(rust_comp::default_run_metaprocessor(out1))
        .tap(move |(expanded, _)| {
            writeln!(expanded_file, "{:?}", expanded).unwrap();
        })
        .tap(move |(expanded, _)| {
            let formatted = formatter::format_stmts_default(expanded);
            writeln!(full_expanded_file, "{}", formatted).unwrap();
        })
        .then(move |(expanded, decl_reg)| {
            let env = Env::new();
            interpreter::eval(&expanded, env, decl_reg, &mut None, &mut out2);
            ()
        });

    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    exec.run(buf)
}
