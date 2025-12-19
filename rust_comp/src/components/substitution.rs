use crate::models::ast::{LoweredExpr, LoweredStmt};
use crate::models::environment::EnvRef;
use crate::models::value::Value;

fn subst_expr(expr: &LoweredExpr, env: &EnvRef) -> LoweredExpr {
    match expr {
        LoweredExpr::Int(n) => LoweredExpr::Int(*n),

        LoweredExpr::String(s) => LoweredExpr::String(s.clone()),

        LoweredExpr::Bool(b) => LoweredExpr::Bool(*b),

        LoweredExpr::Variable(name) => match env.borrow().get(name) {
            Some(Value::Int(n)) => LoweredExpr::Int(n),
            Some(Value::String(s)) => LoweredExpr::String(s.clone()),
            Some(Value::Bool(b)) => LoweredExpr::Bool(b),

            _ => LoweredExpr::Variable(name.clone()),
        },

        LoweredExpr::Add(a, b) => {
            LoweredExpr::Add(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        LoweredExpr::Sub(a, b) => {
            LoweredExpr::Sub(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        LoweredExpr::Mult(a, b) => {
            LoweredExpr::Mult(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        LoweredExpr::Div(a, b) => {
            LoweredExpr::Div(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        LoweredExpr::Equals(a, b) => {
            LoweredExpr::Equals(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        LoweredExpr::Call { callee, args } => LoweredExpr::Call {
            callee: Box::new(subst_expr(callee, env)),
            args: args.iter().map(|a| subst_expr(a, env)).collect(),
        },
    }
}

fn subst_stmt(stmt: &LoweredStmt, env: &EnvRef) -> LoweredStmt {
    match stmt {
        LoweredStmt::ExprStmt(e) => LoweredStmt::ExprStmt(Box::new(subst_expr(e, env))),

        LoweredStmt::Assignment { name, expr } => LoweredStmt::Assignment {
            name: name.clone(),
            expr: Box::new(subst_expr(expr, env)),
        },

        LoweredStmt::Print(e) => LoweredStmt::Print(Box::new(subst_expr(e, env))),

        LoweredStmt::If {
            cond,
            body,
            else_branch,
        } => LoweredStmt::If {
            cond: Box::new(subst_expr(cond, env)),
            body: Box::new(subst_stmt(body, env)),
            else_branch: else_branch.as_ref().map(|b| Box::new(subst_stmt(b, env))),
        },

        LoweredStmt::Block(stmts) => {
            LoweredStmt::Block(stmts.iter().map(|s| subst_stmt(s, env)).collect())
        }

        LoweredStmt::FnDecl { name, params, body } => LoweredStmt::FnDecl {
            name: name.clone(),
            params: params.clone(),
            body: Box::new(subst_stmt(body, env)),
        },

        LoweredStmt::Return(expr) => {
            LoweredStmt::Return(expr.as_ref().map(|e| Box::new(subst_expr(e, env))))
        }

        LoweredStmt::Gen(stmts) => {
            LoweredStmt::Gen(stmts.iter().map(|s| subst_stmt(s, env)).collect())
        }
    }
}

pub fn subst_stmts(stmts: &[LoweredStmt], env: &EnvRef) -> Vec<LoweredStmt> {
    stmts.iter().map(|s| subst_stmt(s, env)).collect()
}
