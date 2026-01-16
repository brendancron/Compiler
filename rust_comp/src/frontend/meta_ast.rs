use std::fmt;

#[derive(Debug, Clone)]
pub struct MetaAst {
    pub stmts: Vec<MetaStmtId>,
}

pub type MetaExprId = usize;
pub type MetaStmtId = usize;

pub struct MetaExpr {
    pub id: usize,
    pub node: MetaExprNode,
}

pub struct MetaStmt {
    pub id: usize,
    pub node: MetaStmtNode,
}


#[derive(Debug, Clone)]
pub enum MetaExprNode {

    // LITERAL REPRESENTATION

    Int(i64),
    String(String),
    Bool(bool),

    StructLiteral {
        type_name: String,
        fields: Vec<(String, MetaExprId)>,
    },

    Variable(String),

    List(Vec<MetaExprId>),


    Call {
        callee: String,
        args: Vec<MetaExprId>,
    },

    Typeof(String),
    
    Embed(String),

    // BINOPS

    Add(MetaExprId, MetaExprId),
    Sub(MetaExprId, MetaExprId),
    Mult(MetaExprId, MetaExprId),
    Div(MetaExprId, MetaExprId),
    Equals(MetaExprId, MetaExprId),
}

#[derive(Debug, Clone)]
pub enum MetaStmtNode {
    
    // RAW EXPR STMTS

    ExprStmt(MetaExprId),
    
    // DECLARATION

    VarDecl {
        name: String,
        expr: MetaExprId,
    },
    
    FnDecl {
        name: String,
        params: Vec<String>,
        body: MetaStmtId,
    },

    StructDecl {
        name: String,
        fields: Vec<MetaFieldDecl>,
    },

    // CONTROL

    If {
        cond: MetaExprId,
        body: MetaStmtId,
        else_branch: Option<MetaStmtId>,
    },

    ForEach {
        var: String,
        iterable: MetaExprId,
        body: MetaStmtId,
    },

    Return(Option<MetaExprId>),
    
    Block(Vec<MetaStmtNode>),

    // UTIL

    Import(String),
    
    // META

    MetaStmt(MetaStmtId),
    Gen(Vec<MetaStmtId>),

    // TEMPORARY

    Print(MetaExprId),
}

pub struct MetaFieldDecl {
    pub field_name: String,
    pub type_name: String,
}
