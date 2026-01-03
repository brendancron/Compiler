#[derive(Debug, Clone)]
pub enum ExpandedExpr {
    Int(i64),
    String(String),
    Bool(bool),

    StructLiteral {
        type_name: String,
        fields: Vec<(String, Box<ExpandedExpr>)>,
    },

    Variable(String),

    List(Vec<ExpandedExpr>),

    Add(Box<ExpandedExpr>, Box<ExpandedExpr>),
    Sub(Box<ExpandedExpr>, Box<ExpandedExpr>),
    Mult(Box<ExpandedExpr>, Box<ExpandedExpr>),
    Div(Box<ExpandedExpr>, Box<ExpandedExpr>),
    Equals(Box<ExpandedExpr>, Box<ExpandedExpr>),

    Call {
        callee: String,
        args: Vec<ExpandedExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum ExpandedStmt {
    ExprStmt(Box<ExpandedExpr>),

    Assignment {
        name: String,
        expr: Box<ExpandedExpr>,
    },

    Print(Box<ExpandedExpr>),

    If {
        cond: Box<ExpandedExpr>,
        body: Box<ExpandedStmt>,
        else_branch: Option<Box<ExpandedStmt>>,
    },

    ForEach {
        var: String,
        iterable: Box<ExpandedExpr>,
        body: Box<ExpandedStmt>,
    },

    Block(Vec<ExpandedStmt>),

    FnDecl {
        name: String,
        params: Vec<String>,
        body: Box<ExpandedStmt>,
    },

    Return(Option<Box<ExpandedExpr>>),

    Gen(Vec<ExpandedStmt>),
}
