use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct MetaAst {
    pub sem_root_stmts: Vec<MetaStmtId>,
    exprs: HashMap<MetaExprId, MetaExprNode>,
    stmts: HashMap<MetaStmtId, MetaStmtNode>,
    current_expr_id: usize,
    current_stmt_id: usize,
}

pub type MetaExprId = usize;
pub type MetaStmtId = usize;

impl MetaAst {

    pub fn new() -> Self {
        Self {
            sem_root_stmts: vec![],
            exprs: HashMap::new(),
            stmts: HashMap::new(),
            current_expr_id: 0,
            current_stmt_id: 0,
        }
    }

    pub fn insert_expr(&mut self, expr: MetaExprNode) -> usize {
        let id = self.current_expr_id;
        self.exprs.insert(id, expr);
        self.current_expr_id += 1;
        id
    }

    pub fn insert_stmt(&mut self, stmt: MetaStmtNode) -> usize {
        let id = self.current_stmt_id;
        self.stmts.insert(id, stmt);
        self.current_stmt_id += 1;
        id
    }

    pub fn get_expr(&self, id: MetaExprId) -> Option<&MetaExprNode> {
        self.exprs.get(&id)
    }

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
    
    Block(Vec<MetaStmtId>),

    // UTIL

    Import(String),
    
    // META

    MetaStmt(MetaStmtId),
    Gen(Vec<MetaStmtId>),

    // TEMPORARY

    Print(MetaExprId),
}

#[derive(Debug, Clone)]
pub struct MetaFieldDecl {
    pub field_name: String,
    pub type_name: String,
}
