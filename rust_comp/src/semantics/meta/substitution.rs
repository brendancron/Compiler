use crate::runtime::environment::*;
use crate::runtime::value::Value;
use crate::semantics::meta::runtime_ast::*;

fn subst_str(name: &str, env: &EnvRef) -> String {
    match env.borrow().get(name) {
        Some(Value::String(s)) => s.clone(),
        _ => name.to_string(),
    }
}

fn subst_expr(expr: &RuntimeExpr, env: &EnvRef) -> RuntimeExpr {
    match expr {
        RuntimeExpr::Int(n) => RuntimeExpr::Int(*n),

        RuntimeExpr::String(s) => RuntimeExpr::String(s.clone()),

        RuntimeExpr::Bool(b) => RuntimeExpr::Bool(*b),

        RuntimeExpr::StructLiteral { type_name, fields } => RuntimeExpr::StructLiteral {
            type_name: subst_str(type_name, env),
            fields: fields.clone(),
        },

        RuntimeExpr::Variable(name) => match env.borrow().get(name) {
            Some(Value::Int(n)) => RuntimeExpr::Int(n),
            Some(Value::String(s)) => RuntimeExpr::String(s.clone()),
            Some(Value::Bool(b)) => RuntimeExpr::Bool(b),

            _ => RuntimeExpr::Variable(name.clone()),
        },

        RuntimeExpr::List(exprs) => RuntimeExpr::List(exprs.clone()),

        RuntimeExpr::Add(a, b) => {
            RuntimeExpr::Add(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        RuntimeExpr::Sub(a, b) => {
            RuntimeExpr::Sub(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        RuntimeExpr::Mult(a, b) => {
            RuntimeExpr::Mult(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        RuntimeExpr::Div(a, b) => {
            RuntimeExpr::Div(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        RuntimeExpr::Equals(a, b) => {
            RuntimeExpr::Equals(Box::new(subst_expr(a, env)), Box::new(subst_expr(b, env)))
        }

        RuntimeExpr::Call { callee, args } => RuntimeExpr::Call {
            callee: subst_str(callee, env),
            args: args.iter().map(|a| subst_expr(a, env)).collect(),
        },
    }
}

fn subst_stmt(stmt: &RuntimeStmt, env: &EnvRef) -> RuntimeStmt {
    match stmt {
        RuntimeStmt::ExprStmt(e) => RuntimeStmt::ExprStmt(Box::new(subst_expr(e, env))),

        RuntimeStmt::Assignment { name, expr } => RuntimeStmt::Assignment {
            name: subst_str(name, env),
            expr: Box::new(subst_expr(expr, env)),
        },

        RuntimeStmt::Print(e) => RuntimeStmt::Print(Box::new(subst_expr(e, env))),

        RuntimeStmt::If {
            cond,
            body,
            else_branch,
        } => RuntimeStmt::If {
            cond: Box::new(subst_expr(cond, env)),
            body: Box::new(subst_stmt(body, env)),
            else_branch: else_branch.as_ref().map(|b| Box::new(subst_stmt(b, env))),
        },

        RuntimeStmt::ForEach {
            var,
            iterable,
            body,
        } => RuntimeStmt::ForEach {
            var: subst_str(var, env),
            iterable: Box::new(subst_expr(iterable, env)),
            body: Box::new(subst_stmt(body, env)),
        },

        RuntimeStmt::Block(stmts) => {
            RuntimeStmt::Block(stmts.iter().map(|s| subst_stmt(s, env)).collect())
        }

        RuntimeStmt::FnDecl { name, params, body } => RuntimeStmt::FnDecl {
            name: subst_str(name, env),
            params: params.clone(),
            body: Box::new(subst_stmt(body, env)),
        },

        RuntimeStmt::Return(expr) => {
            RuntimeStmt::Return(expr.as_ref().map(|e| Box::new(subst_expr(e, env))))
        }

        RuntimeStmt::Gen(stmts) => {
            RuntimeStmt::Gen(stmts.iter().map(|s| subst_stmt(s, env)).collect())
        }
    }
}

pub fn subst_stmts(stmts: &[RuntimeStmt], env: &EnvRef) -> Vec<RuntimeStmt> {
    stmts.iter().map(|s| subst_stmt(s, env)).collect()
}
