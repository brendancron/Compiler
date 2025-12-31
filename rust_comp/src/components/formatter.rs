use crate::models::ast::{LoweredExpr, LoweredStmt, TypeExpr};

#[derive(Debug, Clone)]
pub struct FormatSettings {
    pub indent_size: usize,
    pub indent_string: String,
    pub line_ending: String,
    pub spaces_around_binary_ops: bool,
    pub spaces_inside_parens: bool,
    pub spaces_inside_brackets: bool,
    pub newline_after_block_open: bool,
    pub newline_before_block_close: bool,
}

impl Default for FormatSettings {
    fn default() -> Self {
        Self {
            indent_size: 4,
            indent_string: "    ".to_string(), // 4 spaces
            line_ending: "\n".to_string(),
            spaces_around_binary_ops: true,
            spaces_inside_parens: false,
            spaces_inside_brackets: false,
            newline_after_block_open: true,
            newline_before_block_close: true,
        }
    }
}

pub struct Formatter {
    settings: FormatSettings,
    current_indent: usize,
}

impl Formatter {
    pub fn new(settings: FormatSettings) -> Self {
        Self {
            settings,
            current_indent: 0,
        }
    }

    pub fn with_default_settings() -> Self {
        Self::new(FormatSettings::default())
    }

    fn indent(&self) -> String {
        self.settings.indent_string.repeat(self.current_indent)
    }

    fn indent_increase(&mut self) {
        self.current_indent += 1;
    }

    fn indent_decrease(&mut self) {
        if self.current_indent > 0 {
            self.current_indent -= 1;
        }
    }

    pub fn format_stmt(&mut self, stmt: &LoweredStmt) -> String {
        match stmt {
            LoweredStmt::ExprStmt(expr) => {
                format!("{}{};", self.indent(), self.format_expr(expr))
            }

            LoweredStmt::Assignment { name, expr } => {
                format!(
                    "{}var {} = {};",
                    self.indent(),
                    name,
                    self.format_expr(expr)
                )
            }

            LoweredStmt::Print(expr) => {
                format!("{}print({});", self.indent(), self.format_expr(expr))
            }

            LoweredStmt::If {
                cond,
                body,
                else_branch,
            } => {
                let mut result = format!(
                    "{}if ({}) {}",
                    self.indent(),
                    self.format_expr(cond),
                    if self.settings.newline_after_block_open {
                        format!("{{{}", self.settings.line_ending)
                    } else {
                        "{".to_string()
                    }
                );

                self.indent_increase();
                result.push_str(&self.format_stmt(body));
                self.indent_decrease();

                if let Some(else_stmt) = else_branch {
                    if self.settings.newline_before_block_close {
                        result.push_str(&self.settings.line_ending);
                        result.push_str(&self.indent());
                    }
                    result.push('}');

                    // Handle else-if chains
                    match else_stmt.as_ref() {
                        LoweredStmt::If { .. } => {
                            // For else-if, format recursively but replace leading "if" with "else if"
                            result.push_str(" else ");
                            let saved_indent = self.current_indent;
                            self.current_indent = 0;
                            let mut else_str = self.format_stmt(else_stmt);
                            self.current_indent = saved_indent;

                            // Remove any leading indent and replace "if" with nothing (we already added "else")
                            let indent_prefix = self.settings.indent_string.repeat(saved_indent);
                            if else_str.starts_with(&indent_prefix) {
                                else_str = else_str[indent_prefix.len()..].to_string();
                            }
                            if else_str.starts_with("if ") {
                                else_str = else_str[3..].to_string();
                            }
                            result.push_str(&else_str);
                        }
                        _ => {
                            // Regular else block
                            result.push_str(" else ");
                            result.push_str(&if self.settings.newline_after_block_open {
                                format!("{{{}", self.settings.line_ending)
                            } else {
                                "{".to_string()
                            });
                            self.indent_increase();
                            result.push_str(&self.format_stmt(else_stmt));
                            self.indent_decrease();
                            if self.settings.newline_before_block_close {
                                result.push_str(&self.settings.line_ending);
                                result.push_str(&self.indent());
                            }
                            result.push('}');
                        }
                    }
                } else {
                    if self.settings.newline_before_block_close {
                        result.push_str(&self.settings.line_ending);
                        result.push_str(&self.indent());
                    }
                    result.push('}');
                }

                result
            }

            LoweredStmt::ForEach {
                var,
                iterable,
                body,
            } => {
                let mut result = format!(
                    "{}for ({} in {}) {}",
                    self.indent(),
                    var,
                    self.format_expr(iterable),
                    if self.settings.newline_after_block_open {
                        format!("{{{}", self.settings.line_ending)
                    } else {
                        "{".to_string()
                    }
                );

                self.indent_increase();
                result.push_str(&self.format_stmt(body));
                self.indent_decrease();

                if self.settings.newline_before_block_close {
                    result.push_str(&self.settings.line_ending);
                    result.push_str(&self.indent());
                }
                result.push('}');

                result
            }

            LoweredStmt::Block(stmts) => {
                if stmts.is_empty() {
                    return format!("{}{{}}", self.indent());
                }

                let mut result = if self.settings.newline_after_block_open {
                    format!("{}{{{}", self.indent(), self.settings.line_ending)
                } else {
                    format!("{}{{", self.indent())
                };

                self.indent_increase();
                for stmt in stmts {
                    result.push_str(&self.format_stmt(stmt));
                    result.push_str(&self.settings.line_ending);
                }
                self.indent_decrease();

                if self.settings.newline_before_block_close {
                    result.push_str(&self.indent());
                }
                result.push('}');

                result
            }

            LoweredStmt::FnDecl { name, params, body } => {
                let params_str = params.join(", ");
                let mut result = format!(
                    "{}fn {}({}) {}",
                    self.indent(),
                    name,
                    params_str,
                    if self.settings.newline_after_block_open {
                        format!("{{{}", self.settings.line_ending)
                    } else {
                        "{".to_string()
                    }
                );

                self.indent_increase();
                result.push_str(&self.format_stmt(body));
                self.indent_decrease();

                if self.settings.newline_before_block_close {
                    result.push_str(&self.settings.line_ending);
                    result.push_str(&self.indent());
                }
                result.push('}');

                result
            }

            LoweredStmt::StructDecl { name, fields } => {
                let mut result = format!(
                    "{}struct {} {}",
                    self.indent(),
                    name,
                    if self.settings.newline_after_block_open {
                        format!("{{{}", self.settings.line_ending)
                    } else {
                        "{".to_string()
                    }
                );

                self.indent_increase();
                for (i, (field_name, field_type)) in fields.iter().enumerate() {
                    result.push_str(&self.indent());
                    result.push_str(field_name);
                    result.push_str(": ");
                    result.push_str(&self.format_type(field_type));
                    if i < fields.len() - 1 {
                        result.push(';');
                    }
                    result.push_str(&self.settings.line_ending);
                }
                self.indent_decrease();

                if self.settings.newline_before_block_close {
                    result.push_str(&self.indent());
                }
                result.push('}');

                result
            }

            LoweredStmt::Return(expr) => {
                if let Some(expr) = expr {
                    format!("{}return {};", self.indent(), self.format_expr(expr))
                } else {
                    format!("{}return;", self.indent())
                }
            }

            LoweredStmt::Gen(stmts) => {
                let mut result = format!("{}gen ", self.indent());
                for (i, stmt) in stmts.iter().enumerate() {
                    if i > 0 {
                        result.push(' ');
                    }
                    // Gen statements are typically single expressions
                    result.push_str(&self.format_stmt(stmt));
                }
                result
            }
        }
    }

    pub fn format_expr(&self, expr: &LoweredExpr) -> String {
        match expr {
            LoweredExpr::Int(n) => n.to_string(),
            LoweredExpr::String(s) => format!("\"{}\"", s),
            LoweredExpr::Bool(true) => "true".to_string(),
            LoweredExpr::Bool(false) => "false".to_string(),

            LoweredExpr::Variable(name) => name.clone(),

            LoweredExpr::StructLiteral { type_name, fields } => {
                if fields.is_empty() {
                    format!("{} {{}}", type_name)
                } else {
                    let fields_str = fields
                        .iter()
                        .map(|(name, expr)| format!("{}: {}", name, self.format_expr(expr)))
                        .collect::<Vec<_>>()
                        .join(&format!(",{}", self.settings.line_ending));

                    // For struct literals, we want multi-line format with proper indentation
                    format!(
                        "{} {{{}{}{}}}",
                        type_name, self.settings.line_ending, fields_str, self.settings.line_ending
                    )
                }
            }

            LoweredExpr::List(exprs) => {
                let items_str = exprs
                    .iter()
                    .map(|e| self.format_expr(e))
                    .collect::<Vec<_>>()
                    .join(", ");

                let space_open = if self.settings.spaces_inside_brackets {
                    " "
                } else {
                    ""
                };
                let space_close = if self.settings.spaces_inside_brackets {
                    " "
                } else {
                    ""
                };
                format!("[{}{}{}]", space_open, items_str, space_close)
            }

            LoweredExpr::Add(left, right) => {
                let op = if self.settings.spaces_around_binary_ops {
                    " + "
                } else {
                    "+"
                };
                format!(
                    "{}{}{}",
                    self.format_expr(left),
                    op,
                    self.format_expr(right)
                )
            }

            LoweredExpr::Sub(left, right) => {
                let op = if self.settings.spaces_around_binary_ops {
                    " - "
                } else {
                    "-"
                };
                format!(
                    "{}{}{}",
                    self.format_expr(left),
                    op,
                    self.format_expr(right)
                )
            }

            LoweredExpr::Mult(left, right) => {
                let op = if self.settings.spaces_around_binary_ops {
                    " * "
                } else {
                    "*"
                };
                format!(
                    "{}{}{}",
                    self.format_expr(left),
                    op,
                    self.format_expr(right)
                )
            }

            LoweredExpr::Div(left, right) => {
                let op = if self.settings.spaces_around_binary_ops {
                    " / "
                } else {
                    "/"
                };
                format!(
                    "{}{}{}",
                    self.format_expr(left),
                    op,
                    self.format_expr(right)
                )
            }

            LoweredExpr::Equals(left, right) => {
                let op = if self.settings.spaces_around_binary_ops {
                    " == "
                } else {
                    "=="
                };
                format!(
                    "{}{}{}",
                    self.format_expr(left),
                    op,
                    self.format_expr(right)
                )
            }

            LoweredExpr::Call { callee, args } => {
                let args_str = args
                    .iter()
                    .map(|e| self.format_expr(e))
                    .collect::<Vec<_>>()
                    .join(", ");

                let space_open = if self.settings.spaces_inside_parens {
                    " "
                } else {
                    ""
                };
                let space_close = if self.settings.spaces_inside_parens {
                    " "
                } else {
                    ""
                };
                format!("{}({}{}{})", callee, space_open, args_str, space_close)
            }
        }
    }

    fn format_type(&self, ty: &TypeExpr) -> String {
        match ty {
            TypeExpr::Int => "int".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Named(name) => name.clone(),
        }
    }
}

// Convenience functions
pub fn format_stmts(stmts: &[LoweredStmt], settings: FormatSettings) -> String {
    let mut formatter = Formatter::new(settings);
    stmts
        .iter()
        .map(|stmt| formatter.format_stmt(stmt))
        .collect::<Vec<_>>()
        .join(&formatter.settings.line_ending)
        + &formatter.settings.line_ending
}

pub fn format_stmts_default(stmts: &[LoweredStmt]) -> String {
    format_stmts(stmts, FormatSettings::default())
}

pub fn format_expr(expr: &LoweredExpr, settings: FormatSettings) -> String {
    let formatter = Formatter::new(settings);
    formatter.format_expr(expr)
}

pub fn format_expr_default(expr: &LoweredExpr) -> String {
    format_expr(expr, FormatSettings::default())
}
