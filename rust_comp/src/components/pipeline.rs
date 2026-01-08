use crate::components::embed_resolver::EmbedResolver;
use crate::components::formatter::{self};
use crate::components::interpreter::{self, EvalError};
use crate::components::lexer::{self, ScanError};
use crate::components::metaprocessor::{self, MetaProcessContext, MetaProcessError};
use crate::components::parser::{self, ParseError};
use crate::models::decl_registry::DeclRegistry;
use crate::models::environment::Env;
use crate::models::semantics::blueprint_ast::BlueprintStmt;
use crate::models::semantics::expanded_ast::ExpandedStmt;
use crate::models::token::Token;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;

#[derive(Debug)]
pub enum PipelineError {
    Message(String),
    Scan(ScanError),
    Parse(ParseError),
    Meta(MetaProcessError),
    Eval(EvalError),
}

impl From<String> for PipelineError {
    fn from(error_message: String) -> Self {
        PipelineError::Message(error_message)
    }
}

impl From<&str> for PipelineError {
    fn from(error_message: &str) -> Self {
        PipelineError::Message(error_message.to_string())
    }
}

impl From<ScanError> for PipelineError {
    fn from(e: ScanError) -> Self {
        PipelineError::Scan(e)
    }
}
impl From<ParseError> for PipelineError {
    fn from(e: ParseError) -> Self {
        PipelineError::Parse(e)
    }
}
impl From<MetaProcessError> for PipelineError {
    fn from(e: MetaProcessError) -> Self {
        PipelineError::Meta(e)
    }
}
impl From<EvalError> for PipelineError {
    fn from(e: EvalError) -> Self {
        PipelineError::Eval(e)
    }
}

pub struct Pipeline<I, O> {
    pub exec: Box<dyn FnMut(I, &mut PipelineCtx) -> Result<O, PipelineError>>,
}

impl<I: 'static, O: 'static> Pipeline<I, O> {
    pub fn run(mut self, i: I, ctx: &mut PipelineCtx) -> O {
        (self.exec)(i, ctx).unwrap()
    }
}

impl<I: 'static, O: 'static> Pipeline<I, O> {
    pub fn new<F, E>(mut func: F) -> Self
    where
        F: FnMut(I, &mut PipelineCtx) -> Result<O, E> + 'static,
        E: Into<PipelineError> + 'static,
    {
        Pipeline {
            exec: Box::new(move |input, ctx| func(input, ctx).map_err(Into::into)),
        }
    }

    pub fn then<N: 'static>(self, next: Pipeline<O, N>) -> Pipeline<I, N> {
        let mut step1 = self.exec;
        let mut step2 = next.exec;

        Pipeline {
            exec: Box::new(move |input, ctx| {
                let intermediate = step1(input, ctx)?;
                step2(intermediate, ctx)
            }),
        }
    }
}

impl<I: 'static> Pipeline<I, I> {
    pub fn tap<F, E>(mut func: F) -> Self
    where
        F: FnMut(&I, &mut PipelineCtx) -> Result<(), E> + 'static,
        E: Into<PipelineError> + 'static,
    {
        Pipeline {
            exec: Box::new(move |input, ctx| {
                func(&input, ctx).map_err(Into::into)?;
                Ok(input)
            }),
        }
    }
}

#[derive(Clone)]
pub struct PipelineCtx {
    pub out_dir: PathBuf,
    pub decl_reg: DeclRegistry,
}

pub fn lexer_pipeline() -> Pipeline<String, Vec<Token>> {
    Pipeline::new(|s: String, _ctx| lexer::tokenize(&s))
}

pub fn parser_pipeline() -> Pipeline<Vec<Token>, Vec<BlueprintStmt>> {
    Pipeline::new(|tokens: Vec<Token>, _ctx| parser::parse(&tokens))
}

pub fn metaprocessor_pipeline<E, W>(
    mut out: W,
    mut resolver: E,
) -> Pipeline<Vec<BlueprintStmt>, Vec<ExpandedStmt>>
where
    E: EmbedResolver + 'static,
    W: Write + 'static,
{
    Pipeline::new(move |blueprint, ctx| {
        let meta_env = Env::new();

        let mut meta_process_ctx = MetaProcessContext {
            env: meta_env,
            decls: &mut ctx.decl_reg,
            embed_resolver: &mut resolver,
            out: &mut out,
        };

        let processed = metaprocessor::process(&blueprint, &mut meta_process_ctx)
            .map_err(PipelineError::Meta)?;

        Ok::<_, PipelineError>(processed)
    })
}

pub fn interpreter_pipeline<W>(mut out: W) -> Pipeline<Vec<ExpandedStmt>, ()>
where
    W: Write + 'static,
{
    Pipeline::new(move |expanded, ctx| {
        let env = Env::new();
        interpreter::eval(&expanded, env, &mut ctx.decl_reg, &mut None, &mut out)
            .map_err(|e| PipelineError::Message(format!("{:?}", e)))?;
        Ok::<_, PipelineError>(())
    })
}

pub fn dump_source() -> Pipeline<String, String> {
    Pipeline::tap(move |s, ctx| {
        let out_dir = ctx.out_dir.clone();
        fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
        let mut f = File::create(out_dir.join("source_code.cx")).map_err(|e| e.to_string())?;
        writeln!(f, "{s}").map_err(|e| e.to_string())?;
        Ok::<_, PipelineError>(())
    })
}

pub fn dump_tokens() -> Pipeline<Vec<Token>, Vec<Token>> {
    Pipeline::tap(move |tokens, ctx| {
        let out_dir = ctx.out_dir.clone();
        fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
        let mut f = File::create(out_dir.join("tokens.txt")).map_err(|e| e.to_string())?;
        for t in tokens {
            writeln!(f, "{t:?}").map_err(|e| e.to_string())?;
        }
        Ok::<_, PipelineError>(())
    })
}

pub fn dump_blueprint_ast() -> Pipeline<Vec<BlueprintStmt>, Vec<BlueprintStmt>> {
    Pipeline::tap(move |b, ctx| {
        let out_dir = ctx.out_dir.clone();
        fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
        let mut f = File::create(out_dir.join("parsed_ast.txt")).map_err(|e| e.to_string())?;
        writeln!(f, "{b:?}").map_err(|e| e.to_string())?;
        Ok::<_, PipelineError>(())
    })
}

pub fn dump_expanded_ast() -> Pipeline<Vec<ExpandedStmt>, Vec<ExpandedStmt>> {
    Pipeline::tap(move |expanded, ctx| {
        let out_dir = ctx.out_dir.clone();
        fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
        let mut f = File::create(out_dir.join("expanded_ast.txt")).map_err(|e| e.to_string())?;
        writeln!(f, "{expanded:?}").map_err(|e| e.to_string())?;
        Ok::<_, PipelineError>(())
    })
}

pub fn dump_expanded_code() -> Pipeline<Vec<ExpandedStmt>, Vec<ExpandedStmt>> {
    Pipeline::tap(
        move |expanded: &Vec<ExpandedStmt>, ctx: &mut PipelineCtx| -> Result<(), PipelineError> {
            let out_dir = ctx.out_dir.clone();
            fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
            let mut f =
                File::create(out_dir.join("expanded_ast.txt")).map_err(|e| e.to_string())?;
            let formatted = formatter::format_stmts_default(&expanded);
            writeln!(f, "{formatted}").map_err(|e| e.to_string())?;
            Ok(())
        },
    )
}
