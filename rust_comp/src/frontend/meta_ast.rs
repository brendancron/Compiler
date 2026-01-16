use std::collections::HashMap;
use crate::util::formatters::tree_formatter::*;

#[derive(Debug, Clone)]
pub struct MetaAst {
    pub sem_root_stmts: Vec<MetaStmtId>,
    exprs: HashMap<MetaExprId, MetaExpr>,
    stmts: HashMap<MetaStmtId, MetaStmt>,
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

    pub fn insert_expr(&mut self, expr: MetaExpr) -> usize {
        let id = self.current_expr_id;
        self.exprs.insert(id, expr);
        self.current_expr_id += 1;
        id
    }

    pub fn insert_stmt(&mut self, stmt: MetaStmt) -> usize {
        let id = self.current_stmt_id;
        self.stmts.insert(id, stmt);
        self.current_stmt_id += 1;
        id
    }

    pub fn get_expr(&self, id: MetaExprId) -> Option<&MetaExpr> {
        self.exprs.get(&id)
    }

    pub fn get_stmt(&self, id: MetaStmtId) -> Option<&MetaStmt> {
        self.stmts.get(&id)
    }

}

#[derive(Debug, Clone)]
pub enum MetaExpr {

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
pub enum MetaStmt {
    
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

    MetaBlock(MetaStmtId),
    Gen(Vec<MetaStmtId>),

    // TEMPORARY

    Print(MetaExprId),
}

#[derive(Debug, Clone)]
pub struct MetaFieldDecl {
    pub field_name: String,
    pub type_name: String,
}

impl AsTree for MetaAst {
    fn as_tree(&self) -> Vec<TreeNode> {
        let mut nodes = vec![];
        for stmt_id in self.sem_root_stmts.iter() {
            nodes.push(self.convert_stmt(*stmt_id));
        }
        nodes
    }
}

impl MetaAst {
    fn convert_stmt(&self, id: MetaStmtId) -> TreeNode {
        let stmt = self.get_stmt(id).expect("invalid stmt id");
        match stmt {
            MetaStmt::ExprStmt(e) =>
                TreeNode::node("ExprStmt", vec![self.convert_expr(*e)]),

            MetaStmt::VarDecl { name, expr } =>
                TreeNode::node("VarDecl", vec![
                    TreeNode::leaf(format!("Name({name})")),
                    self.convert_expr(*expr),
                ]),

            MetaStmt::FnDecl { name, params, body } =>
                TreeNode::node("FnDecl", vec![
                    TreeNode::leaf(format!("Name({name})")),
                    TreeNode::node("Params",
                        params.iter()
                            .map(|p| TreeNode::leaf(p.clone()))
                            .collect()),
                    self.convert_stmt(*body),
                ]),

            MetaStmt::StructDecl { name, fields } =>
                TreeNode::node("StructDecl", vec![
                    TreeNode::leaf(format!("Name({name})")),
                    TreeNode::node("Fields",
                        fields.iter().map(|f|
                            TreeNode::leaf(format!("{}: {}", f.field_name, f.type_name))
                        ).collect()),
                ]),

            MetaStmt::If { cond, body, else_branch } =>
                TreeNode::node("IfStmt", {
                    let mut v = vec![
                        TreeNode::node("Cond", vec![self.convert_expr(*cond)]),
                        TreeNode::node("Then", vec![self.convert_stmt(*body)]),
                    ];
                    if let Some(e) = else_branch {
                        v.push(TreeNode::node("Else", vec![self.convert_stmt(*e)]));
                    }
                    v
                }),

            MetaStmt::ForEach { var, iterable, body } =>
                TreeNode::node("ForEachStmt", vec![
                    TreeNode::leaf(format!("Var({var})")),
                    TreeNode::node("Iterable", vec![self.convert_expr(*iterable)]),
                    TreeNode::node("Body", vec![self.convert_stmt(*body)]),
                ]),

            MetaStmt::Return(e) =>
                TreeNode::node("ReturnStmt",
                    e.map(|id| vec![self.convert_expr(id)]).unwrap_or_default()),

            MetaStmt::Block(stmts) =>
                TreeNode::node("Block",
                    stmts.iter().map(|s| self.convert_stmt(*s)).collect()),

            MetaStmt::Import(path) =>
                TreeNode::leaf(format!("Import({path})")),

            MetaStmt::MetaBlock(s) =>
                TreeNode::node("MetaBlock", vec![self.convert_stmt(*s)]),

            MetaStmt::Gen(stmts) =>
                TreeNode::node("Gen",
                    stmts.iter().map(|s| self.convert_stmt(*s)).collect()),

            MetaStmt::Print(e) =>
                TreeNode::node("PrintStmt", vec![self.convert_expr(*e)]),
        }
    }

    fn convert_expr(&self, id: MetaExprId) -> TreeNode {
        let expr = self.get_expr(id).expect("invalid expr id");
        match expr {
            MetaExpr::Int(v) =>
                TreeNode::leaf(format!("Int({v})")),

            MetaExpr::String(s) =>
                TreeNode::leaf(format!("String(\"{s}\")")),

            MetaExpr::Bool(b) =>
                TreeNode::leaf(format!("Bool({b})")),

            MetaExpr::Variable(name) =>
                TreeNode::leaf(format!("Var({name})")),

            MetaExpr::StructLiteral { type_name, fields } =>
                TreeNode::node(format!("StructLiteral({type_name})"),
                    fields.iter().map(|(n, e)|
                        TreeNode::node(n.clone(), vec![self.convert_expr(*e)])
                    ).collect()),

            MetaExpr::List(items) =>
                TreeNode::node("List",
                    items.iter().map(|e| self.convert_expr(*e)).collect()),

            MetaExpr::Call { callee, args } =>
                TreeNode::node(format!("Call({callee})"),
                    args.iter().map(|e| self.convert_expr(*e)).collect()),

            MetaExpr::Typeof(name) =>
                TreeNode::leaf(format!("Typeof({name})")),

            MetaExpr::Embed(path) =>
                TreeNode::leaf(format!("Embed({path})")),

            MetaExpr::Add(a, b) =>
                TreeNode::node("Add", vec![self.convert_expr(*a), self.convert_expr(*b)]),

            MetaExpr::Sub(a, b) =>
                TreeNode::node("Sub", vec![self.convert_expr(*a), self.convert_expr(*b)]),

            MetaExpr::Mult(a, b) =>
                TreeNode::node("Mult", vec![self.convert_expr(*a), self.convert_expr(*b)]),

            MetaExpr::Div(a, b) =>
                TreeNode::node("Div", vec![self.convert_expr(*a), self.convert_expr(*b)]),

            MetaExpr::Equals(a, b) =>
                TreeNode::node("Equals", vec![self.convert_expr(*a), self.convert_expr(*b)]),
        }
    }
}
