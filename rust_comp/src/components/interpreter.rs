use crate::components::metaprocessor::MetaContext;
use crate::components::substitution::subst_stmts;
use crate::models::decl_registry::DeclRegistryRef;
use crate::models::environment::{Env, EnvRef};
use crate::models::result::ExecResult;
use crate::models::semantics::expanded_ast::{ExpandedExpr, ExpandedStmt};
use crate::models::value::{Function, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

pub fn eval_expr<W: Write>(
    expr: &ExpandedExpr,
    env: EnvRef,
    decls: DeclRegistryRef,
    ctx: &mut Option<&mut MetaContext>,
    out: &mut W,
) -> Value {
    match expr {
        ExpandedExpr::Int(n) => Value::Int(*n),
        ExpandedExpr::String(s) => Value::String(s.clone()),
        ExpandedExpr::Bool(b) => Value::Bool(*b),

        ExpandedExpr::StructLiteral { type_name, fields } => {
            let _struct_def = decls
                .borrow()
                .get_struct(type_name)
                .unwrap_or_else(|| panic!("unknown struct type {}", type_name));

            let mut map = HashMap::new();

            for (field_name, expr) in fields {
                let value = eval_expr(expr, env.clone(), decls.clone(), ctx, out);
                map.insert(field_name.clone(), value);
            }

            Value::Struct {
                type_name: type_name.clone(),
                fields: Rc::new(RefCell::new(map)),
            }
        }

        ExpandedExpr::Variable(name) => env
            .borrow()
            .get(name)
            .unwrap_or_else(|| panic!("undefined variable: {}", name)),

        ExpandedExpr::List(exprs) => {
            let values = exprs
                .iter()
                .map(|e| eval_expr(e, env.clone(), decls.clone(), ctx, out))
                .collect();

            Value::List(Rc::new(RefCell::new(values)))
        }

        ExpandedExpr::Add(a, b) => match (
            eval_expr(a, env.clone(), decls.clone(), ctx, out),
            eval_expr(b, env.clone(), decls, ctx, out),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
            (Value::String(x), Value::String(y)) => Value::String(x + &y),
            _ => panic!("type error: + expects ints"),
        },

        ExpandedExpr::Sub(a, b) => match (
            eval_expr(a, env.clone(), decls.clone(), ctx, out),
            eval_expr(b, env.clone(), decls, ctx, out),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
            _ => panic!("type error: - expects ints"),
        },

        ExpandedExpr::Mult(a, b) => match (
            eval_expr(a, env.clone(), decls.clone(), ctx, out),
            eval_expr(b, env.clone(), decls, ctx, out),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
            _ => panic!("type error: * expects ints"),
        },

        ExpandedExpr::Div(a, b) => match (
            eval_expr(a, env.clone(), decls.clone(), ctx, out),
            eval_expr(b, env.clone(), decls, ctx, out),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
            _ => panic!("type error: / expects ints"),
        },

        ExpandedExpr::Equals(a, b) => match (
            eval_expr(a, env.clone(), decls.clone(), ctx, out),
            eval_expr(b, env.clone(), decls, ctx, out),
        ) {
            (Value::Int(x), Value::Int(y)) => Value::Bool(x == y),
            (Value::String(x), Value::String(y)) => Value::Bool(x == y),
            (Value::Bool(x), Value::Bool(y)) => Value::Bool(x == y),
            _ => panic!("type error: == mismatched types"),
        },

        ExpandedExpr::Call { callee, args } => {
            let func = match env.borrow().get(callee) {
                Some(Value::Function(f)) => f,
                _ => panic!("attempted to call a non-function"),
            };

            if func.params.len() != args.len() {
                panic!("wrong number of arguments");
            }

            let arg_vals: Vec<Value> = args
                .iter()
                .map(|a| eval_expr(a, env.clone(), decls.clone(), ctx, out))
                .collect();

            let callee_env = Env::new_child(Rc::clone(&func.env));

            {
                let mut e = callee_env.borrow_mut();
                for (param, value) in func.params.iter().zip(arg_vals) {
                    e.define(param.clone(), value);
                }
            }

            let result = match eval_stmt(&func.body, callee_env, decls, ctx, out) {
                ExecResult::Return(v) => v,
                ExecResult::Continue => Value::Unit,
            };

            result
        }
    }
}

pub fn eval_stmt<W: Write>(
    stmt: &ExpandedStmt,
    env: EnvRef,
    decls: DeclRegistryRef,
    ctx: &mut Option<&mut MetaContext>,
    out: &mut W,
) -> ExecResult {
    match stmt {
        ExpandedStmt::Print(expr) => {
            let value = eval_expr(expr, env.clone(), decls, ctx, out);
            writeln!(out, "{}", value).unwrap();
            ExecResult::Continue
        }

        ExpandedStmt::If {
            cond,
            body,
            else_branch,
        } => match eval_expr(cond, env.clone(), decls.clone(), ctx, out) {
            Value::Bool(true) => eval_stmt(body, env.clone(), decls, ctx, out),
            Value::Bool(false) => match else_branch {
                Some(else_stmt) => eval_stmt(else_stmt, env.clone(), decls, ctx, out),
                None => ExecResult::Continue,
            },
            _ => panic!("type error: expected bool expr"),
        },

        ExpandedStmt::ForEach {
            var,
            iterable,
            body,
        } => {
            let value = eval_expr(iterable, env.clone(), decls.clone(), ctx, out);

            for elem in value.enumerate().iter() {
                env.borrow_mut().push_scope();
                env.borrow_mut().define(var.clone(), elem.clone());

                match eval_stmt(body, env.clone(), decls.clone(), ctx, out) {
                    ExecResult::Return(v) => {
                        env.borrow_mut().pop_scope();
                        return ExecResult::Return(v);
                    }
                    ExecResult::Continue => {}
                }

                env.borrow_mut().pop_scope();
            }

            ExecResult::Continue
        }

        ExpandedStmt::ExprStmt(expr) => {
            eval_expr(expr, env.clone(), decls, ctx, out);
            ExecResult::Continue
        }

        ExpandedStmt::Assignment { name, expr } => {
            let value = eval_expr(expr, env.clone(), decls, ctx, out);
            env.borrow_mut().define(name.clone(), value);
            ExecResult::Continue
        }

        ExpandedStmt::Block(stmts) => {
            env.borrow_mut().push_scope();
            let res = eval(stmts, env.clone(), decls, ctx, out);
            env.borrow_mut().pop_scope();
            res
        }

        ExpandedStmt::FnDecl { name, params, body } => {
            let func = Rc::new(Function {
                params: params.clone(),
                body: body.clone(),
                env: Rc::clone(&env),
            });

            env.borrow_mut().define(name.clone(), Value::Function(func));

            ExecResult::Continue
        }

        ExpandedStmt::Return(opt_expr) => {
            let val = match opt_expr {
                None => Value::Unit,
                Some(expr) => eval_expr(expr, env, decls, ctx, out),
            };
            ExecResult::Return(val)
        }

        ExpandedStmt::Gen(stmts) => {
            let meta = ctx.as_deref_mut().expect("gen outside meta");
            let substituted = subst_stmts(stmts, &env);
            for stmt in substituted {
                meta.emitted.push(stmt.clone());
            }
            ExecResult::Continue
        }
    }
}

pub fn eval<W: Write>(
    stmts: &Vec<ExpandedStmt>,
    env: EnvRef,
    decls: DeclRegistryRef,
    ctx: &mut Option<&mut MetaContext>,
    out: &mut W,
) -> ExecResult {
    for stmt in stmts {
        match eval_stmt(stmt, env.clone(), decls.clone(), ctx, out) {
            ExecResult::Continue => {}
            ExecResult::Return(v) => {
                return ExecResult::Return(v);
            }
        }
    }
    ExecResult::Continue
}
