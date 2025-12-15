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

    If(Box<Expr>, Box<Expr>),
    Assignment(String, Box<Expr>),
    Print(Box<Expr>),
}
