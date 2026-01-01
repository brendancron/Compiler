pub trait Stage<I, O>: 'static {
    fn run(&mut self, input: I) -> O;
}

impl<I, O, F> Stage<I, O> for F
where
    F: FnMut(I) -> O + 'static,
{
    fn run(&mut self, input: I) -> O {
        self(input)
    }
}

pub struct Pipeline<I, O> {
    stage: Box<dyn Stage<I, O>>,
}

impl<I> Pipeline<I, I> {
    pub fn new() -> Self {
        Self {
            stage: Box::new(|x| x),
        }
    }
}

impl<I: 'static, O: 'static> Pipeline<I, O> {
    pub fn run(&mut self, input: I) -> O {
        self.stage.run(input)
    }
}

impl<I: 'static, O: 'static> Pipeline<I, O> {
    pub fn then<N: 'static>(self, mut next: impl Stage<O, N> + 'static) -> Pipeline<I, N> {
        let mut prev = self.stage;

        Pipeline {
            stage: Box::new(move |input: I| {
                let mid = prev.run(input);
                next.run(mid)
            }),
        }
    }

    pub fn tap(self, mut f: impl FnMut(&O) + 'static) -> Self {
        let mut prev = self.stage;

        Pipeline {
            stage: Box::new(move |input: I| {
                let out = prev.run(input);
                f(&out);
                out
            }),
        }
    }
}

pub struct PipelineBuilder<I, O> {
    pub pipeline: Pipeline<I, O>,
}

impl PipelineBuilder<String, String> {
    pub fn new() -> Self {
        Self {
            pipeline: Pipeline::new(),
        }
    }
}

impl<I: 'static, O: 'static> PipelineBuilder<I, O> {
    pub fn with_tap(self, f: impl FnMut(&O) + 'static) -> Self {
        Self {
            pipeline: self.pipeline.tap(f),
        }
    }

    pub fn then<N: 'static>(self, stage: impl Stage<O, N> + 'static) -> PipelineBuilder<I, N> {
        PipelineBuilder {
            pipeline: self.pipeline.then(stage),
        }
    }

    pub fn build(self) -> Pipeline<I, O> {
        self.pipeline
    }
}
