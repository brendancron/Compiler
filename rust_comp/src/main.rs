use std::env;
use std::io::{self, Read};
use std::path::PathBuf;

fn main() {
    let out1 = io::stdout();
    let out2 = io::stdout();

    let out_dir = env::args()
        .skip_while(|a| a != "--out")
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("out"));

    let mut exec = rust_comp::debug_executor(out1, out2, out_dir);
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    exec.run(buf)
}
