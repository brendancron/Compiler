use crate::models::ast::{LoweredExpr, LoweredStmt, ParsedExpr, ParsedStmt};

use crate::components::interpreter;
use crate::models::environment::Env;

pub struct MetaContext {
    pub emitted: Vec<LoweredStmt>,
}

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

pub fn lower_stmt(stmt: &ParsedStmt) -> Vec<LoweredStmt> {
    match stmt {
        ParsedStmt::ExprStmt(expr) => vec![LoweredStmt::ExprStmt(Box::new(lower_expr(expr)))],

        ParsedStmt::Assignment { name, expr } => vec![LoweredStmt::Assignment {
            name: name.clone(),
            expr: Box::new(lower_expr(expr)),
        }],

        ParsedStmt::Print(expr) => vec![LoweredStmt::Print(Box::new(lower_expr(expr)))],

        ParsedStmt::If {
            cond,
            body,
            else_branch,
        } => vec![LoweredStmt::If {
            cond: Box::new(lower_expr(cond)),
            body: Box::new(lower_to_block(body)),
            else_branch: else_branch
                .as_ref()
                .map(|stmt| Box::new(LoweredStmt::Block(lower_stmt(stmt)))),
        }],

        ParsedStmt::Block(stmts) => {
            let mut lowered = Vec::new();
            for stmt in stmts {
                lowered.extend(lower_stmt(stmt));
            }
            vec![LoweredStmt::Block(lowered)]
        }

        ParsedStmt::FnDecl { name, params, body } => vec![LoweredStmt::FnDecl {
            name: name.clone(),
            params: params.clone(),
            body: Box::new(lower_to_block(body)),
        }],

        ParsedStmt::Return(expr) => {
            vec![LoweredStmt::Return(
                expr.as_ref().map(|e| Box::new(lower_expr(e))),
            )]
        }

        ParsedStmt::Gen(stmts) => {
            vec![LoweredStmt::Gen(lower(stmts))]
        }

        ParsedStmt::MetaStmt(parsed_stmt) => {
            let lowered_code = lower_stmt(parsed_stmt);

            let mut env = Env::new();
            let mut ctx = MetaContext {
                emitted: Vec::new(),
            };

            interpreter::eval(&lowered_code, &mut env, &mut Some(&mut ctx));

            ctx.emitted
        }
    }
}

fn lower_to_block(stmt: &ParsedStmt) -> LoweredStmt {
    match stmt {
        ParsedStmt::Block(_) => {
            let lowered = lower_stmt(stmt);
            debug_assert!(
                lowered.len() == 1,
                "block lowering must produce exactly one statement"
            );
            lowered.into_iter().next().unwrap()
        }

        _ => LoweredStmt::Block(lower_stmt(stmt)),
    }
}

pub fn lower(stmts: &Vec<ParsedStmt>) -> Vec<LoweredStmt> {
    let mut out = Vec::new();

    for stmt in stmts {
        out.extend(lower_stmt(stmt));
    }

    out
}
