use std::fmt;

#[derive(Debug, Clone)]
pub enum BlueprintExpr {
    Int(i64),
    String(String),
    Bool(bool),

    StructLiteral {
        type_name: String,
        fields: Vec<(String, Box<BlueprintExpr>)>,
    },

    Variable(String),

    List(Vec<BlueprintExpr>),

    Add(Box<BlueprintExpr>, Box<BlueprintExpr>),
    Sub(Box<BlueprintExpr>, Box<BlueprintExpr>),
    Mult(Box<BlueprintExpr>, Box<BlueprintExpr>),
    Div(Box<BlueprintExpr>, Box<BlueprintExpr>),
    Equals(Box<BlueprintExpr>, Box<BlueprintExpr>),

    Call {
        callee: String,
        args: Vec<BlueprintExpr>,
    },

    Typeof(String),
    
    Mod(String),
    Embed(String),
}

#[derive(Debug, Clone)]
pub enum BlueprintStmt {
    ExprStmt(Box<BlueprintExpr>),
    Assignment {
        name: String,
        expr: Box<BlueprintExpr>,
    },
    Print(Box<BlueprintExpr>),

    If {
        cond: Box<BlueprintExpr>,
        body: Box<BlueprintStmt>,
        else_branch: Option<Box<BlueprintStmt>>,
    },

    ForEach {
        var: String,
        iterable: Box<BlueprintExpr>,
        body: Box<BlueprintStmt>,
    },

    Block(Vec<BlueprintStmt>),

    FnDecl {
        name: String,
        func_type: BlueprintFuncType,
        params: Vec<String>,
        body: Box<BlueprintStmt>,
    },

    StructDecl {
        name: String,
        fields: Vec<(String, TypeExpr)>,
    },

    Return(Option<Box<BlueprintExpr>>),

    Gen(Vec<BlueprintStmt>),

    MetaStmt(Box<BlueprintStmt>),
}

#[derive(Debug, Clone)]
pub enum BlueprintFuncType {
    Normal,
    Meta,
    Pure,
}

impl BlueprintFuncType {
    pub fn can_execute_at_meta(&self) -> bool {
        match self {
            BlueprintFuncType::Meta => true,
            BlueprintFuncType::Pure => true,
            BlueprintFuncType::Normal => false,
        }
    }

    pub fn can_execute_at_runtime(&self) -> bool {
        match self {
            BlueprintFuncType::Normal => true,
            BlueprintFuncType::Pure => true,
            BlueprintFuncType::Meta => false,
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
