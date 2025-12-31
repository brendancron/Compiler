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
    let mut lowered_file = File::create(out_dir.join("../out/lowered_ast.txt")).unwrap();
    let mut full_lowered_file = File::create(out_dir.join("../out/lowered_code.cx")).unwrap();

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
        .then(rust_comp::default_run_metaprocessor(out1))
        .tap(move |(lowered, _)| {
            writeln!(lowered_file, "{lowered:#?}").unwrap();
        })
        .tap(move |(lowered, _)| {
            let formatted_code = formatter::format_stmts_default(&lowered);
            writeln!(full_lowered_file, "{formatted_code}").unwrap();
        })
        .then(move |(lowered, decl_reg)| {
            let env = Env::new();
            interpreter::eval(&lowered, env, decl_reg, &mut None, &mut out2);
        });

    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    exec.run(buf);
}
