#[derive(Debug, Clone)]
pub enum ParsedExpr {
    Int(i64),
    String(String),
    Bool(bool),
    Variable(String),

    Add(Box<ParsedExpr>, Box<ParsedExpr>),
    Sub(Box<ParsedExpr>, Box<ParsedExpr>),
    Mult(Box<ParsedExpr>, Box<ParsedExpr>),
    Div(Box<ParsedExpr>, Box<ParsedExpr>),
    Equals(Box<ParsedExpr>, Box<ParsedExpr>),

    Call {
        callee: Box<ParsedExpr>,
        args: Vec<ParsedExpr>,
    },
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
    Block(Vec<ParsedStmt>),

    FnDecl {
        name: String,
        params: Vec<String>,
        body: Box<ParsedStmt>,
    },

    Return(Option<Box<ParsedExpr>>),

    MetaStmt(Box<ParsedStmt>),
}

// --- LOWERED AST ---

#[derive(Debug, Clone)]
pub enum LoweredExpr {
    Int(i64),
    String(String),
    Bool(bool),
    Variable(String),

    Add(Box<LoweredExpr>, Box<LoweredExpr>),
    Sub(Box<LoweredExpr>, Box<LoweredExpr>),
    Mult(Box<LoweredExpr>, Box<LoweredExpr>),
    Div(Box<LoweredExpr>, Box<LoweredExpr>),
    Equals(Box<LoweredExpr>, Box<LoweredExpr>),

    Call {
        callee: Box<LoweredExpr>,
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
    Block(Vec<LoweredStmt>),

    FnDecl {
        name: String,
        params: Vec<String>,
        body: Box<LoweredStmt>,
    },

    Return(Option<Box<LoweredExpr>>),
}
