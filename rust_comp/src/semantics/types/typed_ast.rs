use super::types::Type;

pub trait ToType {
    fn to_type(&self) -> Type;
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub ty: Type,
    pub kind: TypedExprKind,
}

impl ToType for TypedExpr {
    fn to_type(&self) -> Type {
        return self.ty.clone();
    }
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Int(i64),
    String(String),
    Bool(bool),

    StructLiteral {
        type_name: String,
        fields: Vec<(String, Box<TypedExpr>)>,
    },

    Variable(String),

    List(Vec<TypedExpr>),

    Add(Box<TypedExpr>, Box<TypedExpr>),
    Sub(Box<TypedExpr>, Box<TypedExpr>),
    Mult(Box<TypedExpr>, Box<TypedExpr>),
    Div(Box<TypedExpr>, Box<TypedExpr>),
    Equals(Box<TypedExpr>, Box<TypedExpr>),

    Call {
        callee: String,
        args: Vec<TypedExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum TypedStmt {
    ExprStmt(Box<TypedExpr>),

    Assignment {
        name: String,
        expr: Box<TypedExpr>,
    },

    Print(Box<TypedStmt>),

    If {
        cond: Box<TypedExpr>,
        body: Box<TypedStmt>,
        else_branch: Option<Box<TypedStmt>>,
    },

    ForEach {
        var: String,
        iterable: Box<TypedExpr>,
        body: Box<TypedStmt>,
    },

    Block(Vec<TypedStmt>),

    FnDecl {
        name: String,
        params: Vec<String>,
        body: Box<TypedStmt>,
    },

    Return(Option<Box<TypedExpr>>),

    Gen(Vec<TypedStmt>),
}
