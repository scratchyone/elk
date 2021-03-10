use crate::*;
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    String(String),
    Func(Vec<String>, Vec<Statement>),
    Null,
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, "{}", s),
            Self::Func(args, statements) => write!(f, "fn({})", args.join(", ")),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub value: Value,
    pub name: String,
}
impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}={}", self.name, self.value)
    }
}

pub type VarMap = HashMap<String, Variable>;

pub fn interpret_pub(code: Code) -> VarMap {
    let var_map = VarMap::new();
    interpret_scope(code, var_map)
}
fn interpret_scope(code: Code, var_map: VarMap) -> VarMap {
    let mut var_map = var_map;
    for statement in code.statements {
        interpret_statement(statement, &mut var_map);
    }
    var_map
}
fn interpret_statement(statement: Statement, var_map: &mut VarMap) {
    match statement {
        Statement::Print(expr) => println!("{}", interpret_expression(expr, var_map)),
        Statement::Expr(expr) => {
            interpret_expression(expr, var_map);
        }
        Statement::VarDef(name, expr) | Statement::VarAssign(name, expr) => {
            let value = interpret_expression(expr, var_map);
            var_map.insert(name.clone(), Variable { value, name });
        }
        Statement::FuncDef(name, args, statements) => {
            var_map.insert(
                name.clone(),
                Variable {
                    value: Value::Func(args, statements),
                    name,
                },
            );
        }
    }
}
fn interpret_expression(expression: Expression, var_map: &mut VarMap) -> Value {
    match expression {
        Expression::Int(i) => Value::Int(i),
        Expression::String(s) => Value::String(s),
        Expression::FuncCall(name, args) => {
            if let Some(v) = var_map.get(&name) {
                if let Value::Func(arg_names, code) = v.value.clone() {
                    if arg_names.len() != args.len() {
                        panic!("Expected {} args, got {}", arg_names.len(), args.len());
                    }
                    let mut new_map = var_map.clone();
                    for (name, expr) in arg_names.into_iter().zip(args.into_iter()) {
                        new_map.insert(
                            name.clone(),
                            Variable {
                                name: name,
                                value: interpret_expression(expr, var_map),
                            },
                        );
                    }
                    interpret_scope(Code { statements: code }, new_map);
                    Value::Null
                } else {
                    panic!("{} is not a function", name);
                }
            } else {
                panic!("Function {} not found", name);
            }
        }
        Expression::VariableReference(name) => {
            if let Some(var) = var_map.get(&name) {
                var.value.clone()
            } else {
                panic!("Variable {} not found", name);
            }
        }
    }
}
