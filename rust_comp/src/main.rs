use std::io::{self, Read};

fn main() {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();

    let mut out1 = io::stdout();
    let mut out2 = io::stdout();

    rust_comp::run_source(&buf, &mut out1, &mut out2);
}
