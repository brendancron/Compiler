#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub ty: Type,
    pub kind: TypedExprKind,
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
