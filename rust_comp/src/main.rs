use rust_comp::components::embed_resolver::*;
use rust_comp::components::pipeline::*;
use rust_comp::models::decl_registry::DeclRegistry;
use std::io::{self, Read};
use std::path::PathBuf;

fn main() {
    let resolver = DefaultResolver {
        base_dir: PathBuf::from("."),
    };

    let pipeline = dump_source()
        .then(lexer_pipeline())
        .then(dump_tokens())
        .then(parser_pipeline())
        .then(dump_blueprint_ast())
        .then(metaprocessor_pipeline(io::stdout(), resolver))
        .then(dump_expanded_ast())
        .then(dump_expanded_code())
        .then(interpreter_pipeline(io::stdout()));

    let mut pipeline_ctx = PipelineCtx {
        out_dir: PathBuf::from("../out"),
        decl_reg: DeclRegistry::new(),
    };

    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    pipeline.run(buf, &mut pipeline_ctx);
}
