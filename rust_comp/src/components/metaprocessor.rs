use crate::models::ast::{LoweredExpr, LoweredStmt, ParsedExpr, ParsedStmt};

pub fn lower_expr(expr: &ParsedExpr) -> LoweredExpr {
    match expr {
        ParsedExpr::Int(i) => LoweredExpr::Int(*i),
        ParsedExpr::String(s) => LoweredExpr::String(s.clone()),
        ParsedExpr::Bool(b) => LoweredExpr::Bool(*b),
        ParsedExpr::Variable(name) => LoweredExpr::Variable(name.clone()),

        ParsedExpr::Add(a, b) => LoweredExpr::Add(Box::new(lower_expr(a)), Box::new(lower_expr(b))),

        ParsedExpr::Sub(a, b) => LoweredExpr::Sub(Box::new(lower_expr(a)), Box::new(lower_expr(b))),

        ParsedExpr::Mult(a, b) => {
            LoweredExpr::Mult(Box::new(lower_expr(a)), Box::new(lower_expr(b)))
        }

        ParsedExpr::Div(a, b) => LoweredExpr::Div(Box::new(lower_expr(a)), Box::new(lower_expr(b))),

        ParsedExpr::Equals(a, b) => {
            LoweredExpr::Equals(Box::new(lower_expr(a)), Box::new(lower_expr(b)))
        }

        ParsedExpr::Call { callee, args } => LoweredExpr::Call {
            callee: Box::new(lower_expr(callee)),
            args: args.iter().map(lower_expr).collect(),
        },
    }
}

pub fn lower_stmt(stmt: &ParsedStmt) -> LoweredStmt {
    match stmt {
        ParsedStmt::ExprStmt(expr) => LoweredStmt::ExprStmt(Box::new(lower_expr(expr))),

        ParsedStmt::Assignment { name, expr } => LoweredStmt::Assignment {
            name: name.clone(),
            expr: Box::new(lower_expr(expr)),
        },

        ParsedStmt::Print(expr) => LoweredStmt::Print(Box::new(lower_expr(expr))),

        ParsedStmt::If {
            cond,
            body,
            else_branch,
        } => LoweredStmt::If {
            cond: Box::new(lower_expr(cond)),
            body: Box::new(lower_stmt(body)),
            else_branch: else_branch.as_ref().map(|stmt| Box::new(lower_stmt(stmt))),
        },

        ParsedStmt::Block(stmts) => LoweredStmt::Block(stmts.iter().map(lower_stmt).collect()),

        ParsedStmt::FnDecl { name, params, body } => LoweredStmt::FnDecl {
            name: name.clone(),
            params: params.clone(),
            body: Box::new(lower_stmt(body)),
        },

        ParsedStmt::Return(expr) => {
            LoweredStmt::Return(expr.as_ref().map(|e| Box::new(lower_expr(e))))
        }

        ParsedStmt::MetaStmt(_) => {
            panic!("MetaStmt reached lowering phase — meta execution should have removed it");
        }
    }
}
