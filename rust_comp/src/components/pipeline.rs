use std::marker::PhantomData;
use std::sync::Arc;

/// A pipeline step that transforms Input into Output.
/// It wraps a function: Fn(Input) -> Result<Output, Error>
pub struct Pipeline<Input, Output, Error> {
    func: Arc<dyn Fn(Input) -> Result<Output, Error> + Send + Sync>,
}

impl<Input, Output, Error> Pipeline<Input, Output, Error>
where
    Input: 'static,
    Output: 'static,
    Error: 'static,
{
    /// Create a new pipeline step from a function.
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(Input) -> Result<Output, Error> + Send + Sync + 'static,
    {
        Pipeline {
            func: Arc::new(f),
        }
    }

    /// Execute the pipeline with the given input.
    pub fn execute(&self, input: Input) -> Result<Output, Error> {
        (self.func)(input)
    }

    /// Chain another step after this one.
    pub fn then<NextOutput, F>(self, next: F) -> Pipeline<Input, NextOutput, Error>
    where
        NextOutput: 'static,
        F: Fn(Output) -> Result<NextOutput, Error> + Send + Sync + 'static,
    {
        let current = self.func;
        let next = Arc::new(next);
        Pipeline {
            func: Arc::new(move |input| {
                let intermediate = current(input)?;
                next(intermediate)
            }),
        }
    }

    pub fn tap<F>(self, f: F) -> Self
    where
        F: Fn(&Output) -> Result<(), Error> + Send + Sync + 'static,
    {
        let current = self.func;
        let tap_func = Arc::new(f);
        Pipeline {
            func: Arc::new(move |input| {
                let result = current(input)?;
                tap_func(&result)?;
                Ok(result)
            }),
        }
    }

    /// Alias for execute, for compatibility.
    pub fn run(&self, input: Input) -> Result<Output, Error> {
        self.execute(input)
    }

    /// Finishes the build phase. For this Pipeline implementation, it is an identity.
    pub fn build(self) -> Self {
        self
    }
}

pub struct PipelineBuilder;

impl PipelineBuilder {
    pub fn new() -> Pipeline<String, String, String> {
        // The initial pipeline just passes the input string through.
        Pipeline::new(|s| Ok(s))
    }
}
