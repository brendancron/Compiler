use rust_comp::components::embed_resolver::DefaultResolver;
use rust_comp::components::pipeline::PipelineBuilder;
use std::env;
use std::io::{self, Read};
use std::path::PathBuf;

fn main() {
    let out_dir = env::args()
        .skip_while(|a| a != "--out")
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("out"));

    let mut pipeline = PipelineBuilder::new()
        .dump_source(&out_dir)
        .with_lexer()
        .dump_tokens(&out_dir)
        .with_parser()
        .dump_blueprint_ast(&out_dir)
        .with_metaprocessor(
            io::stdout(),
            DefaultResolver {
                base_dir: PathBuf::from("."),
            },
        )
        .dump_expanded_ast(&out_dir)
        .dump_expanded_code(&out_dir)
        .with_interpreter(io::stdout())
        .build();

    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    pipeline.run(buf)
}
