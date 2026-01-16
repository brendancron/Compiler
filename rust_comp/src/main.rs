use std::path::PathBuf;
use rust_comp::frontend::lexer::*;
use std::fs::{ create_dir_all, read_to_string, File};
use std::fmt::Debug;
use std::io::Write;

fn main() {

    fn run_pipeline(root_path: &PathBuf, out_dir: &PathBuf) {
        let buf = read_to_string(root_path).unwrap();
        create_dir_all(&out_dir).unwrap();

        let tokens = tokenize(&buf).unwrap();
        let mut f = File::create(out_dir.join("tokens.txt")).unwrap();
        dump(&tokens, &mut f);
    }

    let input = std::env::args().nth(1);
    let root_path = PathBuf::from(input.expect("source file path required"));
    let out_path = PathBuf::from("../out");
    run_pipeline(&root_path, &out_path);
}

pub fn dump<T: Debug, W: Write>(
    items: &[T],
    out: &mut W,
) { 
    for item in items {
        writeln!(out, "{item:?}").map_err(|e| e.to_string()).unwrap();
    }
}
