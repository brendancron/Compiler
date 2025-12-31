pub struct Executor<I, O>
where
    I: 'static,
    O: 'static,
{
    f: Box<dyn FnMut(I) -> O>,
}

impl<I> Executor<I, I> {
    pub fn new() -> Self {
        Self { f: Box::new(|x| x) }
    }
}

impl<I, O> Executor<I, O>
where
    I: 'static,
    O: 'static,
{
    pub fn then<N>(self, mut g: impl FnMut(O) -> N + 'static) -> Executor<I, N>
    where
        N: 'static,
    {
        let mut f = self.f;

        Executor {
            f: Box::new(move |input| {
                let mid = f(input);
                g(mid)
            }),
        }
    }
}

impl<I, O> Executor<I, O>
where
    I: 'static,
    O: 'static,
{
    pub fn tap(self, mut t: impl FnMut(&O) + 'static) -> Self {
        let mut f = self.f;

        Executor {
            f: Box::new(move |input| {
                let out = f(input);
                t(&out);
                out
            }),
        }
    }
}

impl<I, O> Executor<I, O> {
    pub fn run(&mut self, input: I) -> O {
        (self.f)(input)
    }
}
