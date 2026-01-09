use rust_comp::components::external_resolver::*;
use rust_comp::components::pipeline::*;
use rust_comp::models::decl_registry::DeclRegistry;
use std::io::{self, Read};
use std::path::PathBuf;

fn main() {

    let pipeline = dump_source()
        .then(lexer_pipeline())
        .then(dump_tokens())
        .then(parser_pipeline())
        .then(dump_blueprint_ast())
        .then(metaprocessor_pipeline(io::stdout(), DefaultResolver{}))
        .then(dump_expanded_ast())
        .then(dump_expanded_code())
        .then(interpreter_pipeline(io::stdout()));


    let input = std::env::args().nth(1);
    let mut buf = String::new();
    let root_dir = match input.as_deref() {
        Some("-") | None => {
            io::stdin().read_to_string(&mut buf).unwrap();
            PathBuf::from(".")
        }
        Some(path) => {
            buf = std::fs::read_to_string(path).unwrap();
            PathBuf::from(path)
                .parent()
                .map(PathBuf::from)
                .unwrap_or_else(|| PathBuf::from("."))
        }
    };

    let mut pipeline_ctx = PipelineCtx {
        out_dir: PathBuf::from("../out"),
        root_dir,
        decl_reg: DeclRegistry::new(),
    };

    pipeline.run(buf, &mut pipeline_ctx);
}
