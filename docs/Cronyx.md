## Language Goals
The main purpose of Cronyx is to provide a simple but powerful language. It should be as easy to learn as Python or JavaScript while also providing modern language features.

Metaprocessing is a core feature in Cronyx. It allows for higher order code to be written allowing for many modern language features such as reflection while keeping runtimes fast.

## Architecture
See [[Phase Architecture]]

The big difference between Cronyx and other popular programming languages is the Phase order. Many languages include a preprocessing stage prior to scanning. While this strategy contains many valuable benefits, it also leads to a 2-language feel where learning macros and codegen is an entirely different feel to the language itself. 

In Cronyx, that phase is delayed until after syntactical analysis and the language essentially transpiles to itself minus a few ast variants. This occurs within the Metaprocessor.

The meta processor reduces the code through [[Compile-Time Evaluation (CTFE)]] which invokes the desired Semantic Analyzer in order to reduce meta blocks. This is a recursive process.

## Type System

Cronyx is Strongly Typed, Statically Typed, and Inferred. 
See [[HM Type System]]

## Memory Management

Not totally sure how I want memory management to work in this language. My immediate inclination is towards GC, but maybe I could allow for different methods of memory management.
