use crate::*;
use std::collections::HashMap;
#[derive(Debug, Clone)]
enum Break {
    ReturnStatement(Value),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    String(String),
    Func(Vec<String>, Vec<RangedT<Statement>>, VarMap),
    Bool(bool),
    Range(Range<i32>),
    Array(Vec<Value>),
    Null,
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, "{}", s),
            Self::Func(args, statements, var_map) => {
                write!(
                    f,
                    "fn({}) with {} global vars",
                    args.join(", "),
                    var_map.len()
                )
            }
            Self::Null => write!(f, "null"),
            Self::Array(values) => write!(
                f,
                "[{}]",
                values
                    .iter()
                    .map(|n| format!("{}", n))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Self::Bool(true) => write!(f, "true"),
            Self::Range(r) => write!(f, "{}..{}", r.start, r.end),
            Self::Bool(false) => write!(f, "false"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    let mut var_map = VarMap::new();
    interpret_scope(code, &mut var_map, vec![]).unwrap()
}
fn interpret_scope(
    code: Code,
    var_map: &mut VarMap,
    locals: Vec<Variable>,
) -> Result<VarMap, Break> {
    let mut new_var_map = var_map.clone();
    for local in locals {
        new_var_map.insert(local.name.clone(), local);
    }
    for statement in code.statements {
        match statement {
            (Statement::Return(expr), _) => Err(Break::ReturnStatement(interpret_expression(
                expr,
                &mut new_var_map,
            )?))?,
            _ => interpret_statement(statement, &mut new_var_map)?,
        }
    }
    for (name, variable) in new_var_map.clone() {
        if let Some(old_var) = var_map.get(&name) {
            if old_var != &variable {
                var_map.insert(name, variable.clone());
            }
        }
    }
    Ok(new_var_map.clone())
}
fn value_to_iter(value: Value, span: TaggedRange) -> Vec<Value> {
    match value {
        Value::Range(r) => r.into_iter().map(|n| Value::Int(n)).collect(),
        Value::Array(items) => items,
        Value::String(items) => items
            .split("")
            .map(|n| Value::String(n.to_string()))
            .collect(),
        t => show_error(format!("{} cannot be converted to an iter", t), &span),
    }
}
fn interpret_statement(statement: RangedT<Statement>, var_map: &mut VarMap) -> Result<(), Break> {
    match statement {
        (Statement::Print(expr), _) => print!("{}", interpret_expression(expr, var_map)?),
        (Statement::Return(_), _) => panic!("Internal interpreter error, return statement should have been handled by calling scope"),
        (Statement::Expr(expr), _) => {
            interpret_expression(expr, var_map)?;
        }
        (Statement::VarDef(name, expr),_) => {
            let value = interpret_expression(expr, var_map)?;
            var_map.insert(name.clone(), Variable { value, name });
        }
        (Statement::VarAssign(name, expr) ,span)=> {
            if !var_map.contains_key(&name) {
                show_error(format!("Variable not defined"),&span);
            }
            let value = interpret_expression(expr, var_map)?;
            var_map.insert(name.clone(), Variable { value, name });
        }
        (Statement::ForIn(name, expr, statements),_) => {
            for value in value_to_iter(interpret_expression(expr.clone(), var_map)?, expr.1) {
                 interpret_scope(
                    Code {
                        statements: statements.clone(),
                    },
                    var_map,
                    vec![Variable {
                        name: name.clone(),
                        value,
                    }],
                )?;
            }
        }
        (Statement::WhileLoop(expr, statements),_) => {
            while match interpret_expression(expr.clone(), var_map)? {
                Value::Bool(v) => v,
                _ => show_error(format!("Boolean expected"), &expr.1),
            } {
                interpret_scope(
                    Code {
                        statements: statements.clone(),
                    },
                    var_map,
                    vec![],
                )?;
            }
        }
        (Statement::FuncDef(name, args, statements),_) => {
            var_map.insert(
                name.clone(),
                Variable {
                    value: Value::Func(args, statements, var_map.clone()),
                    name,
                },
            );
        }
    };
    Ok(())
}
fn interpret_expression(
    expression: RangedT<Expression>,
    var_map: &mut VarMap,
) -> Result<Value, Break> {
    match expression {
        (Expression::Int(i), _) => Ok(Value::Int(i)),
        (Expression::Null, _) => Ok(Value::Null),
        (Expression::Bool(b), _) => Ok(Value::Bool(b)),
        (Expression::String(s), _) => Ok(Value::String(s)),
        (Expression::Array(items), _) => {
            let mut vitems = vec![];
            for item in items {
                vitems.push(interpret_expression(item, var_map)?);
            }
            Ok(Value::Array(vitems))
        }
        (Expression::PropertyAccess(object, property), span) => {
            match interpret_expression(*object, var_map)? {
                Value::Array(arr) => match *property {
                    (Expression::FuncCall(fc, _), _) if fc == "len" => {
                        Ok(Value::Int(arr.len() as i32))
                    }
                    (Expression::FuncCall(fc, args), span) if fc == "join" => Ok(Value::String(
                        arr.iter()
                            .map(|n| format!("{}", n))
                            .collect::<Vec<_>>()
                            .join(
                                match interpret_expression(args[0].clone(), var_map)? {
                                    Value::String(s) => s,
                                    _ => show_error(format!("Join expects a string arg"), &span),
                                }
                                .as_str(),
                            ),
                    )),
                    (Expression::FuncCall(fc, _), span) if fc == "push" => {
                        show_error(format!("idk how to push to that"), &span);
                        Ok(Value::Null)
                    }
                    (_, span) => show_error(format!("Join expects a string arg"), &span),
                },
                Value::String(arr) => match *property {
                    (Expression::FuncCall(fc, _), _) if fc == "len" => {
                        Ok(Value::Int(arr.len() as i32))
                    }
                    (Expression::FuncCall(fc, args), span) if fc == "split" => Ok(Value::Array(
                        match interpret_expression(args[0].clone(), var_map)? {
                            Value::String(s) if s == "" => arr
                                .split_terminator("")
                                .skip(1)
                                .map(|n| Value::String(n.to_string()))
                                .collect(),
                            val => arr
                                .split(
                                    match val {
                                        Value::String(s) => s,
                                        _ => {
                                            show_error(format!("Split expects a string arg"), &span)
                                        }
                                    }
                                    .as_str(),
                                )
                                .map(|n| Value::String(n.to_string()))
                                .collect(),
                        },
                    )),
                    (_, span) => show_error(format!("Property not found"), &span),
                },
                _ => show_error(format!("Property not found"), &span),
            }
        }
        (Expression::ArrayIndex(array, index), _) => {
            match interpret_expression(*array.clone(), var_map)? {
                Value::Array(arr) => match interpret_expression(*index.clone(), var_map)? {
                    Value::Int(i) => Ok(arr[i as usize].clone()),
                    _ => show_error(format!("Cannot index with a non-int"), &index.1),
                },

                v => show_error(format!("Cannot index into {}", v), &array.1),
            }
        }
        (Expression::If(expr, statements), _) => {
            if match interpret_expression(*expr.clone(), var_map)? {
                Value::Bool(v) => v,
                _ => show_error(format!("Boolean expected"), &expr.1),
            } {
                interpret_scope(
                    Code {
                        statements: statements.clone(),
                    },
                    var_map,
                    vec![],
                )?;
            };
            Ok(Value::Null)
        }

        (Expression::Range(v1, v2), span) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Range(i..b)),
                _ => show_error(format!("Range can only be created between ints"), &span),
            }
        }
        (Expression::FuncCall(name, args), span) => {
            if let Some(v) = var_map.get(&name) {
                if let Value::Func(arg_names, code, func_var_map) = v.value.clone() {
                    if arg_names.len() != args.len() {
                        show_error(
                            format!("Expected {} args, got {}", arg_names.len(), args.len()),
                            &span,
                        );
                    }
                    match interpret_scope(
                        Code { statements: code },
                        &mut var_map.clone(),
                        arg_names
                            .into_iter()
                            .zip(args.into_iter())
                            .map(|(name, expr)| Variable {
                                name: name,
                                value: interpret_expression(expr, var_map).unwrap(),
                            })
                            .collect(),
                    ) {
                        Ok(_) => Ok(Value::Null),
                        Err(b) => match b {
                            Break::ReturnStatement(value) => Ok(value),
                        },
                    }
                } else {
                    show_error(format!("{} is not a function", name), &span);
                }
            } else {
                show_error(format!("Function {} not found", name), &span);
            }
        }
        (Expression::VariableReference(name), span) => {
            if let Some(var) = var_map.get(&name) {
                Ok(var.value.clone())
            } else {
                show_error(format!("Variable {} not found", name), &span);
            }
        }
        (Expression::EqCmp(v1, v2), _) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Bool(i == b)),
                (Value::String(i), Value::String(b)) => Ok(Value::Bool(i == b)),
                (Value::Bool(i), Value::Bool(b)) => Ok(Value::Bool(i == b)),
                (Value::Null, Value::Null) => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            }
        }
        (Expression::GtCmp(v1, v2), _) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Bool(i > b)),
                _ => Ok(Value::Bool(false)),
            }
        }
        (Expression::LtCmp(v1, v2), _) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Bool(i < b)),
                _ => Ok(Value::Bool(false)),
            }
        }
        (Expression::GteCmp(v1, v2), _) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Bool(i >= b)),
                _ => Ok(Value::Bool(false)),
            }
        }
        (Expression::LteCmp(v1, v2), _) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Bool(i <= b)),
                _ => Ok(Value::Bool(false)),
            }
        }
        (Expression::Plus(v1, v2), span) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Int(i + b)),
                (a, b) => Ok(Value::String(a.to_string() + &b.to_string())),
                _ => show_error(format!("Cannot add conflicting types"), &span),
            }
        }
        (Expression::Minus(v1, v2), span) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Int(i - b)),
                _ => show_error(format!("Cannot subtract conflicting types"), &span),
            }
        }
        (Expression::Multiply(v1, v2), span) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Int(i * b)),
                (Value::String(i), Value::Int(b)) => Ok(Value::String(i.repeat(b as usize))),
                _ => show_error(format!("Cannot multiply conflicting types"), &span),
            }
        }
        (Expression::Mod(v1, v2), span) => {
            match (
                interpret_expression(*v1, var_map)?,
                interpret_expression(*v2, var_map)?,
            ) {
                (Value::Int(i), Value::Int(b)) => Ok(Value::Int(i % b)),
                _ => show_error(format!("Cannot mod conflicting types"), &span),
            }
        }
    }
}
