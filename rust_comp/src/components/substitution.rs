use crate::models::ast::{ExpandedExpr, ExpandedStmt};
use crate::models::environment::EnvRef;
use crate::models::value::Value;

fn subst_str(name: &str, env: &EnvRef) -> String {
    match env.borrow().get(name) {
        Some(Value::String(s)) => s.clone(),
        _ => name.to_string(),
    }
}

fn subst_expr(expr: &ExpandedExpr, env: &EnvRef) -> ExpandedExpr {
    match expr {
        ExpandedExpr::Int(n) => ExpandedExpr::Int(*n),

        ExpandedExpr::String(s) => ExpandedExpr::String(s.clone()),

        ExpandedExpr::Bool(b) => ExpandedExpr::Bool(*b),

        ExpandedExpr::StructLiteral { type_name, fields } => ExpandedExpr::StructLiteral {
            type_name: subst_str(type_name, env),
            fields: fields.clone(),
        },

        ExpandedExpr::Variable(name) => match env.borrow().get(name) {
            Some(Value::Int(n)) => ExpandedExpr::Int(n),
            Some(Value::String(s)) => ExpandedExpr::String(s.clone()),
            Some(Value::Bool(b)) => ExpandedExpr::Bool(b),

            _ => ExpandedExpr::Variable(name.clone()),
        },

        ExpandedExpr::List(exprs) => ExpandedExpr::List(exprs.clone()),

        ExpandedExpr::Add(a, b) => {
            ExpandedExpr::Add(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        ExpandedExpr::Sub(a, b) => {
            ExpandedExpr::Sub(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        ExpandedExpr::Mult(a, b) => {
            ExpandedExpr::Mult(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        ExpandedExpr::Div(a, b) => {
            ExpandedExpr::Div(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        ExpandedExpr::Equals(a, b) => {
            ExpandedExpr::Equals(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        ExpandedExpr::Call { callee, args } => ExpandedExpr::Call {
            callee: subst_str(callee, env),
            args: args.iter().map(|a| subst_expr(a, env)).collect(),
        },
    }
}

fn subst_stmt(stmt: &ExpandedStmt, env: &EnvRef) -> ExpandedStmt {
    match stmt {
        ExpandedStmt::ExprStmt(e) => ExpandedStmt::ExprStmt(Box::new(subst_expr(e, env))),

        ExpandedStmt::Assignment { name, expr } => ExpandedStmt::Assignment {
            name: subst_str(name, env),
            expr: Box::new(subst_expr(expr, env)),
        },

        ExpandedStmt::Print(e) => ExpandedStmt::Print(Box::new(subst_expr(e, env))),

        ExpandedStmt::If {
            cond,
            body,
            else_branch,
        } => ExpandedStmt::If {
            cond: Box::new(subst_expr(cond, env)),
            body: Box::new(subst_stmt(body, env)),
            else_branch: else_branch.as_ref().map(|b| Box::new(subst_stmt(b, env))),
        },

        ExpandedStmt::ForEach {
            var,
            iterable,
            body,
        } => ExpandedStmt::ForEach {
            var: subst_str(var, env),
            iterable: Box::new(subst_expr(iterable, env)),
            body: Box::new(subst_stmt(body, env)),
        },

        ExpandedStmt::Block(stmts) => {
            ExpandedStmt::Block(stmts.iter().map(|s| subst_stmt(s, env)).collect())
        }

        ExpandedStmt::FnDecl { name, params, body } => ExpandedStmt::FnDecl {
            name: subst_str(name, env),
            params: params.clone(),
            body: Box::new(subst_stmt(body, env)),
        },

        ExpandedStmt::Return(expr) => {
            ExpandedStmt::Return(expr.as_ref().map(|e| Box::new(subst_expr(e, env))))
        }

        ExpandedStmt::Gen(stmts) => {
            ExpandedStmt::Gen(stmts.iter().map(|s| subst_stmt(s, env)).collect())
        }
    }
}

pub fn subst_stmts(stmts: &[ExpandedStmt], env: &EnvRef) -> Vec<ExpandedStmt> {
    stmts.iter().map(|s| subst_stmt(s, env)).collect()
}
