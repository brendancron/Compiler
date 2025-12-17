#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    String(String),
    Bool(bool),
    Variable(String),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),

    Call { callee: Box<Expr>, args: Vec<Expr> },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Box<Expr>),
    Assignment {
        name: String,
        expr: Box<Expr>,
    },
    Print(Box<Expr>),
    If {
        cond: Box<Expr>,
        body: Box<Stmt>,
    },
    Block(Vec<Stmt>),

    FnDecl {
        name: String,
        params: Vec<String>,
        body: Box<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Stmt,
}
