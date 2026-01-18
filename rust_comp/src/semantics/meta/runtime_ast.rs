use std::collections::HashMap;
use crate::util::formatters::tree_formatter::*;

#[derive(Debug, Clone)]
pub struct RuntimeAst {
    pub sem_root_stmts: Vec<RuntimeStmtId>,
    exprs: HashMap<RuntimeExprId, RuntimeExpr>,
    stmts: HashMap<RuntimeStmtId, RuntimeStmt>,
    current_expr_id: usize,
    current_stmt_id: usize,
}

pub type RuntimeExprId = usize;
pub type RuntimeStmtId = usize;

impl RuntimeAst {

    pub fn new() -> Self {
        Self {
            sem_root_stmts: vec![],
            exprs: HashMap::new(),
            stmts: HashMap::new(),
            current_expr_id: 0,
            current_stmt_id: 0,
        }
    }

    pub fn insert_expr(&mut self, expr: RuntimeExpr) -> usize {
        let id = self.current_expr_id;
        self.exprs.insert(id, expr);
        self.current_expr_id += 1;
        id
    }

    pub fn insert_stmt(&mut self, stmt: RuntimeStmt) -> usize {
        let id = self.current_stmt_id;
        self.stmts.insert(id, stmt);
        self.current_stmt_id += 1;
        id
    }

    pub fn get_expr(&self, id: RuntimeExprId) -> Option<&RuntimeExpr> {
        self.exprs.get(&id)
    }

    pub fn get_stmt(&self, id: RuntimeStmtId) -> Option<&RuntimeStmt> {
        self.stmts.get(&id)
    }

}

#[derive(Debug, Clone)]
pub enum RuntimeExpr {

    // LITERAL REPRESENTATION

    Int(i64),
    String(String),
    Bool(bool),

    StructLiteral {
        type_name: String,
        fields: Vec<(String, RuntimeExprId)>,
    },

    Variable(String),

    List(Vec<RuntimeExprId>),


    Call {
        callee: String,
        args: Vec<RuntimeExprId>,
    },

    // BINOPS

    Add(RuntimeExprId, RuntimeExprId),
    Sub(RuntimeExprId, RuntimeExprId),
    Mult(RuntimeExprId, RuntimeExprId),
    Div(RuntimeExprId, RuntimeExprId),
    Equals(RuntimeExprId, RuntimeExprId),
}

#[derive(Debug, Clone)]
pub enum RuntimeStmt {
    
    // RAW EXPR STMTS

    ExprStmt(RuntimeExprId),
    
    // DECLARATION

    VarDecl {
        name: String,
        expr: RuntimeExprId,
    },
    
    FnDecl {
        name: String,
        params: Vec<String>,
        body: RuntimeStmtId,
    },

    StructDecl {
        name: String,
        fields: Vec<RuntimeFieldDecl>,
    },

    // CONTROL

    If {
        cond: RuntimeExprId,
        body: RuntimeStmtId,
        else_branch: Option<RuntimeStmtId>,
    },

    ForEach {
        var: String,
        iterable: RuntimeExprId,
        body: RuntimeStmtId,
    },

    Return(Option<RuntimeExprId>),
    
    Block(Vec<RuntimeStmtId>),

    // UTIL

    Import(String),
    
    // META

    Gen(Vec<RuntimeStmtId>),

    // TEMPORARY

    Print(RuntimeExprId),
}

#[derive(Debug, Clone)]
pub struct RuntimeFieldDecl {
    pub field_name: String,
    pub type_name: String,
}

impl AsTree for RuntimeAst {
    fn as_tree(&self) -> Vec<TreeNode> {
        let mut nodes = vec![];
        for stmt_id in self.sem_root_stmts.iter() {
            nodes.push(self.convert_stmt(*stmt_id));
        }
        nodes
    }
}

impl RuntimeAst {
    fn convert_stmt(&self, id: RuntimeStmtId) -> TreeNode {
        let stmt = self.get_stmt(id).expect("invalid stmt id");

        let (label, mut children): (String, Vec<TreeNode>) = match stmt {
            RuntimeStmt::ExprStmt(e) => (
                "ExprStmt".into(),
                vec![self.convert_expr(*e)],
            ),

            RuntimeStmt::VarDecl { name, expr } => (
                "VarDecl".into(),
                vec![
                    TreeNode::leaf(format!("Name({name})")),
                    self.convert_expr(*expr),
                ],
            ),

            RuntimeStmt::FnDecl { name, params, body } => (
                "FnDecl".into(),
                vec![
                    TreeNode::leaf(format!("Name({name})")),
                    TreeNode::node(
                        "Params",
                        params.iter().map(|p| TreeNode::leaf(p.clone())).collect(),
                    ),
                    self.convert_stmt(*body),
                ],
            ),

            RuntimeStmt::StructDecl { name, fields } => (
                "StructDecl".into(),
                vec![
                    TreeNode::leaf(format!("Name({name})")),
                    TreeNode::node(
                        "Fields",
                        fields.iter()
                            .map(|f| TreeNode::leaf(format!("{}: {}", f.field_name, f.type_name)))
                            .collect(),
                    ),
                ],
            ),

            RuntimeStmt::If { cond, body, else_branch } => {
                let mut v = vec![
                    TreeNode::node("Cond", vec![self.convert_expr(*cond)]),
                    TreeNode::node("Then", vec![self.convert_stmt(*body)]),
                ];
                if let Some(e) = else_branch {
                    v.push(TreeNode::node("Else", vec![self.convert_stmt(*e)]));
                }
                ("IfStmt".into(), v)
            }

            RuntimeStmt::ForEach { var, iterable, body } => (
                "ForEachStmt".into(),
                vec![
                    TreeNode::leaf(format!("Var({var})")),
                    TreeNode::node("Iterable", vec![self.convert_expr(*iterable)]),
                    TreeNode::node("Body", vec![self.convert_stmt(*body)]),
                ],
            ),

            RuntimeStmt::Return(e) => (
                "ReturnStmt".into(),
                e.map(|id| vec![self.convert_expr(id)]).unwrap_or_default(),
            ),

            RuntimeStmt::Block(stmts) => (
                "Block".into(),
                stmts.iter().map(|s| self.convert_stmt(*s)).collect(),
            ),

            RuntimeStmt::Import(path) => (
                "Import".into(),
                vec![TreeNode::leaf(path.clone())],
            ),

            RuntimeStmt::Gen(stmts) => (
                "Gen".into(),
                stmts.iter().map(|s| self.convert_stmt(*s)).collect(),
            ),

            RuntimeStmt::Print(e) => (
                "PrintStmt".into(),
                vec![self.convert_expr(*e)],
            ),
        };

        children.insert(0, TreeNode::leaf(format!("id: {id}")));
        TreeNode::node(label, children)
    }

    fn convert_expr(&self, id: RuntimeExprId) -> TreeNode {
        let expr = self.get_expr(id).expect("invalid expr id");

        let (label, mut children) = match expr {
            RuntimeExpr::Int(v) => (
                "Int".into(),
                vec![TreeNode::leaf(v.to_string())],
            ),

            RuntimeExpr::String(s) => (
                "String".into(),
                vec![TreeNode::leaf(format!("\"{s}\""))],
            ),

            RuntimeExpr::Bool(b) => (
                "Bool".into(),
                vec![TreeNode::leaf(b.to_string())],
            ),

            RuntimeExpr::Variable(name) => (
                "Var".into(),
                vec![TreeNode::leaf(name.clone())],
            ),

            RuntimeExpr::StructLiteral { type_name, fields } => (
                format!("StructLiteral({type_name})"),
                fields.iter()
                    .map(|(n, e)| TreeNode::node(n.clone(), vec![self.convert_expr(*e)]))
                    .collect(),
            ),

            RuntimeExpr::List(items) => (
                "List".into(),
                items.iter().map(|e| self.convert_expr(*e)).collect(),
            ),

            RuntimeExpr::Call { callee, args } => (
                format!("Call({callee})"),
                args.iter().map(|e| self.convert_expr(*e)).collect(),
            ),

            RuntimeExpr::Add(a, b) => (
                "Add".into(),
                vec![self.convert_expr(*a), self.convert_expr(*b)],
            ),

            RuntimeExpr::Sub(a, b) => (
                "Sub".into(),
                vec![self.convert_expr(*a), self.convert_expr(*b)],
            ),

            RuntimeExpr::Mult(a, b) => (
                "Mult".into(),
                vec![self.convert_expr(*a), self.convert_expr(*b)],
            ),

            RuntimeExpr::Div(a, b) => (
                "Div".into(),
                vec![self.convert_expr(*a), self.convert_expr(*b)],
            ),

            RuntimeExpr::Equals(a, b) => (
                "Equals".into(),
                vec![self.convert_expr(*a), self.convert_expr(*b)],
            ),
        };

        children.insert(0, TreeNode::leaf(format!("id: {id}")));
        TreeNode::node(label, children)
    }
}
