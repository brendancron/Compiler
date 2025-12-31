use std::fmt;

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    Int(i64),
    String(String),
    Bool(bool),

    StructLiteral {
        type_name: String,
        fields: Vec<(String, Box<ParsedExpr>)>,
    },

    Variable(String),

    List(Vec<ParsedExpr>),

    Add(Box<ParsedExpr>, Box<ParsedExpr>),
    Sub(Box<ParsedExpr>, Box<ParsedExpr>),
    Mult(Box<ParsedExpr>, Box<ParsedExpr>),
    Div(Box<ParsedExpr>, Box<ParsedExpr>),
    Equals(Box<ParsedExpr>, Box<ParsedExpr>),

    Call {
        callee: String,
        args: Vec<ParsedExpr>,
    },

    Typeof(String),
}

#[derive(Debug, Clone)]
pub enum ParsedStmt {
    ExprStmt(Box<ParsedExpr>),
    Assignment {
        name: String,
        expr: Box<ParsedExpr>,
    },
    Print(Box<ParsedExpr>),

    If {
        cond: Box<ParsedExpr>,
        body: Box<ParsedStmt>,
        else_branch: Option<Box<ParsedStmt>>,
    },

    ForEach {
        var: String,
        iterable: Box<ParsedExpr>,
        body: Box<ParsedStmt>,
    },

    Block(Vec<ParsedStmt>),

    FnDecl {
        name: String,
        func_type: ParsedFuncType,
        params: Vec<String>,
        body: Box<ParsedStmt>,
    },

    StructDecl {
        name: String,
        fields: Vec<(String, TypeExpr)>,
    },

    Return(Option<Box<ParsedExpr>>),

    Gen(Vec<ParsedStmt>),

    MetaStmt(Box<ParsedStmt>),
}

// --- LOWERED AST ---

#[derive(Debug, Clone)]
pub enum LoweredExpr {
    Int(i64),
    String(String),
    Bool(bool),

    StructLiteral {
        type_name: String,
        fields: Vec<(String, Box<LoweredExpr>)>,
    },

    Variable(String),

    List(Vec<LoweredExpr>),

    Add(Box<LoweredExpr>, Box<LoweredExpr>),
    Sub(Box<LoweredExpr>, Box<LoweredExpr>),
    Mult(Box<LoweredExpr>, Box<LoweredExpr>),
    Div(Box<LoweredExpr>, Box<LoweredExpr>),
    Equals(Box<LoweredExpr>, Box<LoweredExpr>),

    Call {
        callee: String,
        args: Vec<LoweredExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum LoweredStmt {
    ExprStmt(Box<LoweredExpr>),
    Assignment {
        name: String,
        expr: Box<LoweredExpr>,
    },
    Print(Box<LoweredExpr>),
    If {
        cond: Box<LoweredExpr>,
        body: Box<LoweredStmt>,
        else_branch: Option<Box<LoweredStmt>>,
    },

    ForEach {
        var: String,
        iterable: Box<LoweredExpr>,
        body: Box<LoweredStmt>,
    },

    Block(Vec<LoweredStmt>),

    FnDecl {
        name: String,
        params: Vec<String>,
        body: Box<LoweredStmt>,
    },

    Return(Option<Box<LoweredExpr>>),

    Gen(Vec<LoweredStmt>),
}

#[derive(Debug, Clone)]
pub enum ParsedFuncType {
    Normal,
    Meta,
    Pure,
}

impl ParsedFuncType {
    pub fn can_execute_at_meta(&self) -> bool {
        match self {
            ParsedFuncType::Meta => true,
            ParsedFuncType::Pure => true,
            ParsedFuncType::Normal => false,
        }
    }

    pub fn can_execute_at_runtime(&self) -> bool {
        match self {
            ParsedFuncType::Normal => true,
            ParsedFuncType::Pure => true,
            ParsedFuncType::Meta => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Int,
    String,
    Bool,
    Named(String),
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::Int => write!(f, "int"),
            TypeExpr::String => write!(f, "string"),
            TypeExpr::Bool => write!(f, "bool"),
            TypeExpr::Named(name) => write!(f, "{name}"),
        }
    }
}
