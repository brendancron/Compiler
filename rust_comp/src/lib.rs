pub mod components {
    pub mod embed_resolver;
    pub mod formatter;
    pub mod interpreter;
    pub mod lexer;
    pub mod metaprocessor;
    pub mod parser;
    pub mod substitution;
    pub mod type_checker;
    pub mod pipeline;
}

pub mod models {
    pub mod semantics {
        pub mod blueprint_ast;
        pub mod expanded_ast;
        pub mod typed_ast;
    }
    pub mod decl_registry;
    pub mod environment;
    pub mod result;
    pub mod token;
    pub mod types {
        pub mod type_env;
        pub mod type_error;
        pub mod type_subst;
        pub mod type_utils;
        pub mod types;
    }
    pub mod value;
}

use components::embed_resolver::EmbedResolver;
use components::{formatter, pipeline::{Pipeline, PipelineBuilder}};
use components::lexer::{self};
use components::interpreter::{self};
use components::parser::{self};
use components::metaprocessor::{self, MetaProcessContext};
use models::decl_registry::{DeclRegistry, DeclRegistryRef};
use models::environment::Env;
use models::semantics::blueprint_ast::BlueprintStmt;
use models::semantics::expanded_ast::ExpandedStmt;
use models::token::Token;
use std::fs::{self, File};
use std::io::{Write};
use std::path::PathBuf;

pub type CompilerError = String;

impl Pipeline<String, String, CompilerError> {
    pub fn with_lexer(self) -> Pipeline<String, Vec<Token>, CompilerError> {
        self.then(|s| lexer::tokenize(&s).map_err(|e| format!("{:?}", e)))
    }
}

impl Pipeline<String, Vec<Token>, CompilerError> {
    pub fn with_parser(self) -> Pipeline<String, Vec<BlueprintStmt>, CompilerError> {
        self.then(|t| parser::parse(&t).map_err(|e| format!("{:?}", e)))
    }
}

impl Pipeline<String, Vec<BlueprintStmt>, CompilerError> {
    pub fn with_metaprocessor<E, W>(
        self,
        mut out: W,
        mut resolver: E,
    ) -> Pipeline<String, (Vec<ExpandedStmt>, DeclRegistryRef), CompilerError>
    where
        E: EmbedResolver + 'static,
        W: Write + 'static,
    {
        self.then(move |parsed| {
            let meta_env = Env::new();
            let decl_reg = DeclRegistry::new();

            let mut mp_ctx = MetaProcessContext {
                env: meta_env,
                decls: decl_reg.clone(),
                embed_resolver: &mut resolver,
                out: &mut out,
            };

            let processed = metaprocessor::process(&parsed, &mut mp_ctx).map_err(|e| format!("{:?}", e))?;
            Ok((processed, decl_reg))
        })
    }
}

impl Pipeline<String, (Vec<ExpandedStmt>, DeclRegistryRef), CompilerError> {
    pub fn with_interpreter<W>(
        self,
        mut out: W,
    ) -> Pipeline<String, (), CompilerError> 
    where
        W: Write + 'static,
    {
        self.then(move |(expanded, decl_reg)| {
            let env = Env::new();
            interpreter::eval(&expanded, env, decl_reg, &mut None, &mut out).map_err(|e| format!("{:?}", e))?;
            Ok(())
        })
    }
}

impl Pipeline<String, String, CompilerError> {
    pub fn dump_source<P: AsRef<std::path::Path>>(self, out_dir: P) -> Self {
        let out_dir = out_dir.as_ref().to_path_buf();
        self.tap(move |s| {
            fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
            let mut f = File::create(out_dir.join("source_code.cx")).map_err(|e| e.to_string())?;
            writeln!(f, "{s}").map_err(|e| e.to_string())?;
            Ok(())
        })
    }
}

impl Pipeline<String, Vec<Token>, CompilerError> {
    pub fn dump_tokens<P: AsRef<std::path::Path>>(self, out_dir: P) -> Self {
        let out_dir = out_dir.as_ref().to_path_buf();
        self.tap(move |tokens| {
            fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
            let mut f = File::create(out_dir.join("tokens.txt")).map_err(|e| e.to_string())?;
            for t in tokens { writeln!(f, "{t:?}").map_err(|e| e.to_string())?; }
            Ok(())
        })
    }
}

impl Pipeline<String, Vec<BlueprintStmt>, CompilerError> {
    pub fn dump_blueprint_ast<P: AsRef<std::path::Path>>(self, out_dir: P) -> Self {
        let out_dir = out_dir.as_ref().to_path_buf();
        self.tap(move |b| {
            fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
            let mut f = File::create(out_dir.join("parsed_ast.txt")).map_err(|e| e.to_string())?;
            writeln!(f, "{b:?}").map_err(|e| e.to_string())?;
            Ok(())
        })
    }
}

impl Pipeline<String, (Vec<ExpandedStmt>, DeclRegistryRef), CompilerError> {
    pub fn dump_expanded_ast<P: AsRef<std::path::Path>>(self, out_dir: P) -> Self {
        let out_dir = out_dir.as_ref().to_path_buf();
        self.tap(move |(expanded, _)| {
            fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
            let mut f = File::create(out_dir.join("expanded_ast.txt")).map_err(|e| e.to_string())?;
            writeln!(f, "{expanded:?}").map_err(|e| e.to_string())?;
            Ok(())
        })
    }

    pub fn dump_expanded_code<P: AsRef<std::path::Path>>(self, out_dir: P) -> Self {
        let out_dir = out_dir.as_ref().to_path_buf();
        self.tap(move |(expanded, _)| {
            fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
            let mut f = File::create(out_dir.join("expanded_code.cx")).map_err(|e| e.to_string())?;
            let formatted = formatter::format_stmts_default(expanded);
            writeln!(f, "{formatted}").map_err(|e| e.to_string())?;
            Ok(())
        })
    }
}

pub fn default_pipeline<M, E, R>(source: &str, meta_out: M, eval_out: E, resolver: R) -> Pipeline<String, (), CompilerError>
where
    M: Write + Clone + Send + Sync + 'static,
    E: Write + Clone + Send + Sync + 'static,
    R: EmbedResolver + Clone + Send + Sync + 'static,
{
    // Note: The usage of clone here implies the writer/resolver should be cloneable or wrapped in Arc/Mutex if they have state.
    // However, the original request didn't specify the traits constraints heavily, so we assume they satisfy 'static + Send + Sync.
    // If Write is not Clone, we might need a RefCell or Arc<Mutex<W>> wrapper, but for now we follow the pattern.
    // Actually, Pipeline::then takes `move`, so the writer is moved into the closure.
    // Since we are building the pipeline *once* and returning it, we can move the arguments in.
    
    // BUT the function signature 'source: &str' is unused in the builder pattern which takes input at 'execute'.
    // The previous implementation of `default_pipeline` returned `Pipeline<String, ...>` but it seems the intention is to build a pipeline to be executed later OR execute it?
    // The previous code returned `Pipeline<String, ()>`.
    
    PipelineBuilder::new()
        .with_lexer()
        .with_parser()
        .with_metaprocessor(meta_out, resolver)
        .with_interpreter(eval_out)
}

pub fn debug_pipeline<M, E, R>(
    meta_out: M,
    eval_out: E,
    resolver: R,
    out_dir: PathBuf,
) -> Pipeline<String, (), CompilerError>
where
    M: Write + Send + Sync + 'static,
    E: Write + Send + Sync + 'static,
    R: EmbedResolver + Send + Sync + 'static,
{
     PipelineBuilder::new()
        .dump_source(out_dir.clone())
        .with_lexer()
        .dump_tokens(out_dir.clone())
        .with_parser()
        .dump_blueprint_ast(out_dir.clone())
        .with_metaprocessor(meta_out, resolver)
        .dump_expanded_ast(out_dir.clone())
        .dump_expanded_code(out_dir.clone())
        .with_interpreter(eval_out)
}
