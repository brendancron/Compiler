use std::fs;
use std::io;
use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn normalize(s: &str) -> String {
    s.trim().replace("\r\n", "\n")
}

#[test]
fn run_language_tests() {
    let root = repo_root();
    let cases = root.join("tests/cases");
    let expected = root.join("tests/expected");

    for entry in fs::read_dir(&cases).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|s| s.to_str()) != Some("cx") {
            continue;
        }

        let name = path.file_stem().unwrap().to_str().unwrap();
        let source = fs::read_to_string(&path).unwrap();
        let expected_out = fs::read_to_string(expected.join(format!("{name}.out"))).unwrap();

        let mut meta_out = io::stdout();
        let mut eval_out = Vec::new();

        rust_comp::run_source(&source, &mut meta_out, &mut eval_out);

        let actual = String::from_utf8(eval_out).unwrap();

        assert_eq!(
            normalize(&actual),
            normalize(&expected_out),
            "failed test: {name}"
        );
    }
}
