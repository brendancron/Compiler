pub mod components {
    pub mod external_resolver;
    pub mod formatter;
    pub mod interpreter;
    pub mod lexer;
    pub mod metaprocessor;
    pub mod parser;
    pub mod pipeline;
    pub mod substitution;
    pub mod type_checker;
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

pub mod config;
