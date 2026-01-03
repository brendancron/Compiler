use crate::components::interpreter;
use crate::models::decl_registry::{DeclRegistryRef, StructDef};
use crate::models::environment::EnvRef;
use crate::models::semantics::blueprint_ast::{BlueprintExpr, BlueprintStmt};
use crate::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use crate::models::value::{Function, Value};
use std::io::Write;
use std::rc::Rc;

pub struct MetaContext {
    pub emitted: Vec<ExpandedStmt>,
}

pub fn value_to_literal(val: Value) -> ExpandedExpr {
    match val {
        Value::Int(n) => ExpandedExpr::Int(n),
        Value::String(s) => ExpandedExpr::String(s),
        Value::Bool(b) => ExpandedExpr::Bool(b),
        Value::Unit => panic!("Unit has no literal representation"),
        _ => panic!("non-primitive value not supported yet"),
    }
}

pub fn process_expr<W: Write>(
    expr: &BlueprintExpr,
    env: EnvRef,
    decls: DeclRegistryRef,
    out: &mut W,
) -> ExpandedExpr {
    match expr {
        BlueprintExpr::Int(i) => ExpandedExpr::Int(*i),
        BlueprintExpr::String(s) => ExpandedExpr::String(s.clone()),
        BlueprintExpr::Bool(b) => ExpandedExpr::Bool(*b),

        BlueprintExpr::StructLiteral { type_name, fields } => ExpandedExpr::StructLiteral {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, expr)| {
                    (
                        name.clone(),
                        Box::new(process_expr(expr, env.clone(), decls.clone(), out)),
                    )
                })
                .collect(),
        },

        BlueprintExpr::Variable(name) => match env.borrow().get(name) {
            Some(x) => value_to_literal(x),
            None => ExpandedExpr::Variable(name.clone()),
        },

        BlueprintExpr::List(exprs) => {
            ExpandedExpr::List(process_exprs(exprs, env.clone(), decls, out))
        }

        BlueprintExpr::Add(a, b) => ExpandedExpr::Add(
            Box::new(process_expr(a, env.clone(), decls.clone(), out)),
            Box::new(process_expr(b, env.clone(), decls, out)),
        ),

        BlueprintExpr::Sub(a, b) => ExpandedExpr::Sub(
            Box::new(process_expr(a, env.clone(), decls.clone(), out)),
            Box::new(process_expr(b, env.clone(), decls, out)),
        ),

        BlueprintExpr::Mult(a, b) => ExpandedExpr::Mult(
            Box::new(process_expr(a, env.clone(), decls.clone(), out)),
            Box::new(process_expr(b, env.clone(), decls, out)),
        ),

        BlueprintExpr::Div(a, b) => ExpandedExpr::Div(
            Box::new(process_expr(a, env.clone(), decls.clone(), out)),
            Box::new(process_expr(b, env.clone(), decls, out)),
        ),

        BlueprintExpr::Equals(a, b) => ExpandedExpr::Equals(
            Box::new(process_expr(a, env.clone(), decls.clone(), out)),
            Box::new(process_expr(b, env.clone(), decls, out)),
        ),

        BlueprintExpr::Call { callee, args } => {
            let call_expr = ExpandedExpr::Call {
                callee: callee.clone(),
                args: args
                    .iter()
                    .map(|e| process_expr(e, env.clone(), decls.clone(), out))
                    .collect(),
            };

            match env.borrow().get(&callee) {
                Some(_) => {
                    let val =
                        interpreter::eval_expr(&call_expr, env.clone(), decls, &mut None, out);
                    value_to_literal(val)
                }
                None => call_expr,
            }
        }

        BlueprintExpr::Typeof(id) => {
            let def = decls
                .borrow()
                .get_struct(id)
                .unwrap_or_else(|| panic!("unknown type {}", id));
            ExpandedExpr::String(def.to_string())
        }
    }
}

pub fn process_exprs<W: Write>(
    exprs: &Vec<BlueprintExpr>,
    env: EnvRef,
    decls: DeclRegistryRef,
    out: &mut W,
) -> Vec<ExpandedExpr> {
    let mut output = Vec::new();

    for expr in exprs {
        output.push(process_expr(expr, env.clone(), decls.clone(), out));
    }

    output
}

pub fn process_stmt<W: Write>(
    stmt: &BlueprintStmt,
    env: EnvRef,
    decls: DeclRegistryRef,
    out: &mut W,
) -> Vec<ExpandedStmt> {
    match stmt {
        BlueprintStmt::ExprStmt(expr) => vec![ExpandedStmt::ExprStmt(Box::new(process_expr(
            expr,
            env.clone(),
            decls,
            out,
        )))],

        BlueprintStmt::Assignment { name, expr } => vec![ExpandedStmt::Assignment {
            name: name.clone(),
            expr: Box::new(process_expr(expr, env.clone(), decls, out)),
        }],

        BlueprintStmt::Print(expr) => {
            vec![ExpandedStmt::Print(Box::new(process_expr(
                expr,
                env.clone(),
                decls,
                out,
            )))]
        }

        BlueprintStmt::If {
            cond,
            body,
            else_branch,
        } => vec![ExpandedStmt::If {
            cond: Box::new(process_expr(cond, env.clone(), decls.clone(), out)),
            body: Box::new(process_to_block(body, env.clone(), decls.clone(), out)),
            else_branch: else_branch.as_ref().map(|stmt| {
                Box::new(ExpandedStmt::Block(process_stmt(
                    stmt,
                    env.clone(),
                    decls,
                    out,
                )))
            }),
        }],

        BlueprintStmt::ForEach {
            var,
            iterable,
            body,
        } => vec![ExpandedStmt::ForEach {
            var: var.clone(),
            iterable: Box::new(process_expr(iterable, env.clone(), decls.clone(), out)),
            body: Box::new(process_to_block(body, env.clone(), decls, out)),
        }],

        BlueprintStmt::Block(stmts) => {
            let mut processed = Vec::new();
            for stmt in stmts {
                processed.extend(process_stmt(stmt, env.clone(), decls.clone(), out));
            }
            vec![ExpandedStmt::Block(processed)]
        }

        BlueprintStmt::FnDecl {
            name,
            func_type,
            params,
            body,
        } => {
            let processed_body = process_to_block(body.as_ref(), env.clone(), decls, out);
            if func_type.can_execute_at_meta() {
                let func = Rc::new(Function {
                    params: params.clone(),
                    body: Box::new(processed_body.clone()),
                    env: Rc::clone(&env),
                });
                env.borrow_mut().define(name.clone(), Value::Function(func));
            }

            if func_type.can_execute_at_runtime() {
                let func_decl = ExpandedStmt::FnDecl {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(processed_body.clone()),
                };
                return vec![func_decl];
            }
            vec![]
        }

        BlueprintStmt::StructDecl { name, fields } => {
            decls.borrow_mut().define_struct(
                name.clone(),
                StructDef {
                    fields: fields.clone(),
                },
            );
            vec![]
        }

        BlueprintStmt::Return(expr) => {
            vec![ExpandedStmt::Return(
                expr.as_ref()
                    .map(|e| Box::new(process_expr(e, env.clone(), decls, out))),
            )]
        }

        BlueprintStmt::Gen(stmts) => {
            vec![ExpandedStmt::Gen(process(stmts, env.clone(), decls, out))]
        }

        BlueprintStmt::MetaStmt(parsed_stmt) => {
            let processed_code = process_stmt(parsed_stmt, env.clone(), decls.clone(), out);
            let mut ctx = MetaContext {
                emitted: Vec::new(),
            };

            interpreter::eval(&processed_code, env, decls, &mut Some(&mut ctx), out);

            ctx.emitted
        }
    }
}

fn process_to_block<W: Write>(
    stmt: &BlueprintStmt,
    env: EnvRef,
    decls: DeclRegistryRef,
    out: &mut W,
) -> ExpandedStmt {
    match stmt {
        BlueprintStmt::Block(_) => {
            let processed = process_stmt(stmt, env.clone(), decls, out);
            debug_assert!(
                processed.len() == 1,
                "block processing must produce exactly one statement"
            );
            processed.into_iter().next().unwrap()
        }

        _ => ExpandedStmt::Block(process_stmt(stmt, env.clone(), decls, out)),
    }
}

pub fn process<W: Write>(
    stmts: &Vec<BlueprintStmt>,
    env: EnvRef,
    decls: DeclRegistryRef,
    out: &mut W,
) -> Vec<ExpandedStmt> {
    let mut output = Vec::new();

    for stmt in stmts {
        output.extend(process_stmt(stmt, env.clone(), decls.clone(), out));
    }

    output
}
