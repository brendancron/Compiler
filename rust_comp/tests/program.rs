use rust_comp::components::embed_resolver::DefaultResolver;
use rust_comp::components::pipeline::*;
use rust_comp::models::decl_registry::DeclRegistry;
use std::cell::RefCell;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone)]
pub struct SharedBuf(Rc<RefCell<Vec<u8>>>);

impl SharedBuf {
    pub fn new() -> Self {
        SharedBuf(Rc::new(RefCell::new(Vec::new())))
    }

    pub fn into_inner(self) -> Rc<RefCell<Vec<u8>>> {
        self.0
    }
}

impl Write for SharedBuf {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.borrow_mut().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

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

    let mut failures = Vec::new();

    for entry in fs::read_dir(&cases).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|s| s.to_str()) != Some("cx") {
            continue;
        }

        let name = path.file_stem().unwrap().to_string_lossy();
        let source = fs::read_to_string(&path).unwrap();
        let expected_path = expected.join(format!("{name}.out"));
        let expected_out = fs::read_to_string(&expected_path).unwrap_or_else(|e| {
            panic!("missing expected file: {} ({})", expected_path.display(), e)
        });

        let meta_out = std::io::sink();
        let eval_buf = SharedBuf::new();
        let eval_handle = eval_buf.clone();

        let resolver = DefaultResolver {
            base_dir: PathBuf::from("."),
        };

        let pipeline = lexer_pipeline()
            .then(parser_pipeline())
            .then(metaprocessor_pipeline(meta_out, resolver))
            .then(interpreter_pipeline(eval_handle));

        let mut pipeline_ctx = PipelineCtx {
            out_dir: PathBuf::from("../out"),
            decl_reg: DeclRegistry::new(),
        };

        pipeline.run(source, &mut pipeline_ctx);

        let actual = String::from_utf8(eval_buf.into_inner().borrow().clone()).unwrap();

        if normalize(&actual) != normalize(&expected_out) {
            failures.push(name.to_string());
        }
    }

    if !failures.is_empty() {
        panic!("failed tests:\n{}", failures.join("\n"));
    }
}
