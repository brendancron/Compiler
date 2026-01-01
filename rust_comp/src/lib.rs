pub mod components {
    pub mod formatter;
    pub mod interpreter;
    pub mod lexer;
    pub mod metaprocessor;
    pub mod parser;
    pub mod pipeline;
    pub mod substitution;
}

pub mod models {
    pub mod ast;
    pub mod decl_registry;
    pub mod environment;
    pub mod result;
    pub mod token;
    pub mod types;
    pub mod value;
}

use components::pipeline::{Pipeline, PipelineBuilder};
use components::{formatter, interpreter, lexer, metaprocessor, parser};
use models::ast::{BlueprintStmt, ExpandedStmt};
use models::decl_registry::{DeclRegistry, DeclRegistryRef};
use models::environment::Env;
use models::token::Token;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;

pub type CompilerError = String;

impl PipelineBuilder<String, String> {
    pub fn with_lexer(self) -> PipelineBuilder<String, Vec<Token>> {
        PipelineBuilder {
            pipeline: self.pipeline.then(|s: String| lexer::tokenize(&s)),
        }
    }
}

impl PipelineBuilder<String, Vec<Token>> {
    pub fn with_parser(self) -> PipelineBuilder<String, Vec<BlueprintStmt>> {
        PipelineBuilder {
            pipeline: self.pipeline.then(|t: Vec<Token>| parser::parse(&t)),
        }
    }
}

impl PipelineBuilder<String, Vec<BlueprintStmt>> {
    pub fn with_metaprocessor<W: Write + 'static>(
        self,
        mut out: W,
    ) -> PipelineBuilder<String, (Vec<ExpandedStmt>, DeclRegistryRef)> {
        PipelineBuilder {
            pipeline: self.pipeline.then(move |parsed: Vec<BlueprintStmt>| {
                let meta_env = Env::new();
                let decl_reg = DeclRegistry::new();
                let processed =
                    metaprocessor::process(&parsed, meta_env, decl_reg.clone(), &mut out);
                (processed, decl_reg)
            }),
        }
    }
}

impl PipelineBuilder<String, (Vec<ExpandedStmt>, DeclRegistryRef)> {
    pub fn with_interpreter<E: Write + 'static>(
        self,
        mut eval_out: E,
    ) -> PipelineBuilder<String, ()> {
        PipelineBuilder {
            pipeline: self.pipeline.then(move |(expanded, decl_reg)| {
                let env = Env::new();
                interpreter::eval(&expanded, env, decl_reg, &mut None, &mut eval_out);
            }),
        }
    }
}

impl PipelineBuilder<String, String> {
    pub fn dump_source(self, out_dir: &PathBuf) -> Self {
        fs::create_dir_all(out_dir).unwrap();
        let mut f = File::create(out_dir.join("source_code.cx")).unwrap();

        self.with_tap(move |s: &String| {
            writeln!(f, "{s}").unwrap();
        })
    }
}

impl PipelineBuilder<String, Vec<Token>> {
    pub fn dump_tokens(self, out_dir: &PathBuf) -> Self {
        fs::create_dir_all(out_dir).unwrap();
        let mut f = File::create(out_dir.join("tokens.txt")).unwrap();

        self.with_tap(move |t: &Vec<Token>| {
            writeln!(f, "{t:?}").unwrap();
        })
    }
}

impl PipelineBuilder<String, Vec<BlueprintStmt>> {
    pub fn dump_blueprint_ast(self, out_dir: &PathBuf) -> Self {
        fs::create_dir_all(out_dir).unwrap();
        let mut f = File::create(out_dir.join("parsed_ast.txt")).unwrap();

        self.with_tap(move |b: &Vec<BlueprintStmt>| {
            writeln!(f, "{b:?}").unwrap();
        })
    }
}

impl PipelineBuilder<String, (Vec<ExpandedStmt>, DeclRegistryRef)> {
    pub fn dump_expanded_ast(self, out_dir: &PathBuf) -> Self {
        fs::create_dir_all(out_dir).unwrap();
        let mut f = File::create(out_dir.join("expanded_ast.txt")).unwrap();

        self.with_tap(move |(expanded, _)| {
            writeln!(f, "{expanded:?}").unwrap();
        })
    }

    pub fn dump_expanded_code(self, out_dir: &PathBuf) -> Self {
        fs::create_dir_all(out_dir).unwrap();
        let mut f = File::create(out_dir.join("expanded_code.cx")).unwrap();

        self.with_tap(move |(expanded, _)| {
            let formatted = formatter::format_stmts_default(expanded);
            writeln!(f, "{formatted}").unwrap();
        })
    }
}

pub fn default_pipeline<M, E>(meta_out: M, eval_out: E) -> Pipeline<String, ()>
where
    M: Write + 'static,
    E: Write + 'static,
{
    PipelineBuilder::new()
        .with_lexer()
        .with_parser()
        .with_metaprocessor(meta_out)
        .with_interpreter(eval_out)
        .build()
}

pub fn debug_pipeline<M, E>(meta_out: M, eval_out: E, out_dir: PathBuf) -> Pipeline<String, ()>
where
    M: Write + 'static,
    E: Write + 'static,
{
    PipelineBuilder::new()
        .dump_source(&out_dir)
        .with_lexer()
        .dump_tokens(&out_dir)
        .with_parser()
        .dump_blueprint_ast(&out_dir)
        .with_metaprocessor(meta_out)
        .dump_expanded_ast(&out_dir)
        .dump_expanded_code(&out_dir)
        .with_interpreter(eval_out)
        .build()
}
