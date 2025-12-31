use crate::components::interpreter;
use crate::models::ast::{LoweredExpr, LoweredStmt, ParsedExpr, ParsedStmt};
use crate::models::environment::EnvRef;
use crate::models::value::{Function, Value};
use std::io::Write;
use std::rc::Rc;

pub struct MetaContext {
    pub emitted: Vec<LoweredStmt>,
}

pub fn value_to_literal(val: Value) -> LoweredExpr {
    match val {
        Value::Int(n) => LoweredExpr::Int(n),
        Value::String(s) => LoweredExpr::String(s),
        Value::Bool(b) => LoweredExpr::Bool(b),
        Value::Unit => panic!("Unit has no literal representation"),
        _ => panic!("non-primitive value not supported yet"),
    }
}

pub fn lower_expr<W: Write>(expr: &ParsedExpr, env: EnvRef, out: &mut W) -> LoweredExpr {
    match expr {
        ParsedExpr::Int(i) => LoweredExpr::Int(*i),
        ParsedExpr::String(s) => LoweredExpr::String(s.clone()),
        ParsedExpr::Bool(b) => LoweredExpr::Bool(*b),

        ParsedExpr::StructLiteral { type_name, fields } => LoweredExpr::StructLiteral {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, expr)| (name.clone(), Box::new(lower_expr(expr, env.clone(), out))))
                .collect(),
        },

        ParsedExpr::Variable(name) => match env.borrow().get(name) {
            Some(x) => value_to_literal(x),
            None => LoweredExpr::Variable(name.clone()),
        },

        ParsedExpr::List(exprs) => LoweredExpr::List(lower_exprs(exprs, env.clone(), out)),

        ParsedExpr::Add(a, b) => LoweredExpr::Add(
            Box::new(lower_expr(a, env.clone(), out)),
            Box::new(lower_expr(b, env.clone(), out)),
        ),

        ParsedExpr::Sub(a, b) => LoweredExpr::Sub(
            Box::new(lower_expr(a, env.clone(), out)),
            Box::new(lower_expr(b, env.clone(), out)),
        ),

        ParsedExpr::Mult(a, b) => LoweredExpr::Mult(
            Box::new(lower_expr(a, env.clone(), out)),
            Box::new(lower_expr(b, env.clone(), out)),
        ),

        ParsedExpr::Div(a, b) => LoweredExpr::Div(
            Box::new(lower_expr(a, env.clone(), out)),
            Box::new(lower_expr(b, env.clone(), out)),
        ),

        ParsedExpr::Equals(a, b) => LoweredExpr::Equals(
            Box::new(lower_expr(a, env.clone(), out)),
            Box::new(lower_expr(b, env.clone(), out)),
        ),

        ParsedExpr::Call { callee, args } => {
            let call_expr = LoweredExpr::Call {
                callee: callee.clone(),
                args: args
                    .iter()
                    .map(|e| lower_expr(e, env.clone(), out))
                    .collect(),
            };

            match env.borrow().get(&callee) {
                Some(_) => {
                    let val = interpreter::eval_expr(&call_expr, env.clone(), &mut None, out);
                    value_to_literal(val)
                }
                None => call_expr,
            }
        }
    }
}

pub fn lower_exprs<W: Write>(
    exprs: &Vec<ParsedExpr>,
    env: EnvRef,
    out: &mut W,
) -> Vec<LoweredExpr> {
    let mut output = Vec::new();

    for expr in exprs {
        output.push(lower_expr(expr, env.clone(), out));
    }

    output
}

pub fn lower_stmt<W: Write>(stmt: &ParsedStmt, env: EnvRef, out: &mut W) -> Vec<LoweredStmt> {
    match stmt {
        ParsedStmt::ExprStmt(expr) => vec![LoweredStmt::ExprStmt(Box::new(lower_expr(
            expr,
            env.clone(),
            out,
        )))],

        ParsedStmt::Assignment { name, expr } => vec![LoweredStmt::Assignment {
            name: name.clone(),
            expr: Box::new(lower_expr(expr, env.clone(), out)),
        }],

        ParsedStmt::Print(expr) => {
            vec![LoweredStmt::Print(Box::new(lower_expr(
                expr,
                env.clone(),
                out,
            )))]
        }

        ParsedStmt::If {
            cond,
            body,
            else_branch,
        } => vec![LoweredStmt::If {
            cond: Box::new(lower_expr(cond, env.clone(), out)),
            body: Box::new(lower_to_block(body, env.clone(), out)),
            else_branch: else_branch
                .as_ref()
                .map(|stmt| Box::new(LoweredStmt::Block(lower_stmt(stmt, env.clone(), out)))),
        }],

        ParsedStmt::ForEach {
            var,
            iterable,
            body,
        } => vec![LoweredStmt::ForEach {
            var: var.clone(),
            iterable: Box::new(lower_expr(iterable, env.clone(), out)),
            body: Box::new(lower_to_block(body, env.clone(), out)),
        }],

        ParsedStmt::Block(stmts) => {
            let mut lowered = Vec::new();
            for stmt in stmts {
                lowered.extend(lower_stmt(stmt, env.clone(), out));
            }
            vec![LoweredStmt::Block(lowered)]
        }

        ParsedStmt::FnDecl {
            name,
            func_type,
            params,
            body,
        } => {
            let lowered_body = lower_to_block(body.as_ref(), env.clone(), out);
            if func_type.can_execute_at_meta() {
                let func = Rc::new(Function {
                    params: params.clone(),
                    body: Box::new(lowered_body.clone()),
                    env: Rc::clone(&env),
                });
                env.borrow_mut().define(name.clone(), Value::Function(func));
            }

            if func_type.can_execute_at_runtime() {
                let func_decl = LoweredStmt::FnDecl {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(lowered_body.clone()),
                };
                return vec![func_decl];
            }
            vec![]
        }

        ParsedStmt::StructDecl { name, fields } => vec![LoweredStmt::StructDecl {
            name: name.clone(),
            fields: fields.clone(),
        }],

        ParsedStmt::Return(expr) => {
            vec![LoweredStmt::Return(
                expr.as_ref()
                    .map(|e| Box::new(lower_expr(e, env.clone(), out))),
            )]
        }

        ParsedStmt::Gen(stmts) => {
            vec![LoweredStmt::Gen(lower(stmts, env.clone(), out))]
        }

        ParsedStmt::MetaStmt(parsed_stmt) => {
            let lowered_code = lower_stmt(parsed_stmt, env.clone(), out);
            let mut ctx = MetaContext {
                emitted: Vec::new(),
            };

            interpreter::eval(&lowered_code, env, &mut Some(&mut ctx), out);

            ctx.emitted
        }
    }
}

fn lower_to_block<W: Write>(stmt: &ParsedStmt, env: EnvRef, out: &mut W) -> LoweredStmt {
    match stmt {
        ParsedStmt::Block(_) => {
            let lowered = lower_stmt(stmt, env.clone(), out);
            debug_assert!(
                lowered.len() == 1,
                "block lowering must produce exactly one statement"
            );
            lowered.into_iter().next().unwrap()
        }

        _ => LoweredStmt::Block(lower_stmt(stmt, env.clone(), out)),
    }
}

pub fn lower<W: Write>(stmts: &Vec<ParsedStmt>, env: EnvRef, out: &mut W) -> Vec<LoweredStmt> {
    let mut output = Vec::new();

    for stmt in stmts {
        output.extend(lower_stmt(stmt, env.clone(), out));
    }

    output
}
