use codespan_reporting::term;
use logos::Logos;
use std::io::prelude::*;
use std::{fs::File, ops::Range};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("__print")]
    Print,

    #[token("function ")]
    Function,

    #[token("return ")]
    Return,

    #[token("for")]
    For,

    #[token("if")]
    If,

    #[token(" in ")]
    In,

    #[token("while ")]
    While,

    #[token("use ")]
    Use,

    #[token("let ")]
    Let,

    #[token(";\n")]
    EOL,

    #[token("=")]
    Equal,

    #[token("+")]
    Plus,

    #[token("*")]
    Multiply,

    #[token("-")]
    Minus,

    #[token("[")]
    OpenBracket,

    #[token("]")]
    CloseBracket,

    #[regex("\n")]
    Newline,

    #[token(",")]
    Comma,

    #[token("(")]
    OpenParen,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("..")]
    Range,

    #[token("<")]
    LessThan,

    #[token(">")]
    GreaterThan,

    #[token("<=")]
    LessThanEq,

    #[token(">=")]
    GreaterThanEq,

    #[token("==")]
    DoubleEq,

    #[token(")")]
    CloseParen,

    #[token("//")]
    Comment,

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[token(".")]
    Period,

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let slice = lex.slice();
        let n = slice[1..slice.len() - 1].to_string()
            .replace("\\\"", "\"").replace("\\n", "\n"); // Remove quotes
        Some(n)
    })]
    StringLiteral(String),

    #[regex("[0-9]+", |lex| lex.slice().to_string().parse::<i32>().expect("Failed to parse int literal"))]
    Int(i32),

    #[regex("[a-zA-Z_][a-zA-Z_0-9]*", |v| v.slice().to_string())]
    ObjectName(String),

    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,
}

#[derive(Clone)]
pub struct ParserStream<T, A> {
    pub data: Vec<T>,
    pub default: T,
    pub meta: A,
}
impl<T, A> ParserStream<T, A>
where
    T: Clone,
{
    pub fn new(data: Vec<T>, default: T, meta: A) -> Self {
        Self {
            data,
            default,
            meta,
        }
    }
    pub fn empty(&self) -> bool {
        self.data.len() == 0
    }
    pub fn peek(&self, by: usize) -> &T {
        if by >= self.data.len() {
            &self.default
        } else {
            &self.data[by]
        }
    }
    pub fn consume(&mut self, by: usize) -> T {
        if by >= self.data.len() {
            self.default.clone()
        } else {
            self.data.remove(by)
        }
    }
}

#[derive(Debug)]
pub struct Code {
    pub statements: Vec<Statement>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Print(Expression),
    Expr(Expression),
    VarDef(String, Expression),
    VarAssign(String, Expression),
    FuncDef(String, Vec<String>, Vec<Statement>),
    WhileLoop(Expression, Vec<Statement>),
    ForIn(String, Expression, Vec<Statement>),
    Return(Expression),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Int(i32),
    String(String),
    FuncCall(String, Vec<Expression>),
    VariableReference(String),
    Bool(bool),
    EqCmp(Box<Expression>, Box<Expression>),
    GtCmp(Box<Expression>, Box<Expression>),
    LtCmp(Box<Expression>, Box<Expression>),
    GteCmp(Box<Expression>, Box<Expression>),
    LteCmp(Box<Expression>, Box<Expression>),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Range(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Vec<Statement>),
    Array(Vec<Expression>),
    ArrayIndex(Box<Expression>, Box<Expression>),
    PropertyAccess(Box<Expression>, Box<Expression>),
}

pub fn parse_code(tokens: ParserStream<(Token, Range<usize>), String>) -> Code {
    let mut tokens = preprocess(tokens);
    println!(
        "{:#?}",
        tokens.data.iter().map(|n| n.0.clone()).collect::<Vec<_>>()
    );
    let mut code = Code { statements: vec![] };
    while !tokens.empty() {
        code.statements.push(parse_statement_outer(&mut tokens));
    }
    return code;
}
fn preprocess(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> ParserStream<(Token, Range<usize>), String> {
    let mut new_tokens = ParserStream::new(vec![], tokens.default.clone(), tokens.meta.clone());
    let mut tokens = tokens;
    let mut in_comment = false;
    while !tokens.data.is_empty() {
        let (token, range) = tokens.consume(0);
        if token == Token::Comment {
            in_comment = true;
            continue;
        }
        if token == Token::Newline {
            in_comment = false;
            continue;
        }
        if token == Token::Use {
            let (path, file_location_span) = match tokens.consume(0) {
                (Token::StringLiteral(path), s) => (path, s),
                (_, s) => show_error(
                    format!("Expected path to elk file"),
                    &s,
                    tokens.meta.clone(),
                ),
            };
            let mut file = match File::open(path) {
                Ok(f) => f,
                Err(e) => show_error(format!("{}", e), &file_location_span, tokens.meta.clone()),
            };
            let mut contents = String::new();
            match file.read_to_string(&mut contents) {
                Ok(_) => {}
                Err(e) => show_error(format!("{}", e), &file_location_span, tokens.meta.clone()),
            }
            if !contents.ends_with("\n") {
                contents += "\n";
            }
            match tokens.consume(0) {
                (Token::EOL, _) => {}
                (_, s) => {
                    show_error(
                        format!("Expected semicolon and newline"),
                        &s,
                        tokens.meta.clone(),
                    );
                }
            };
            let lex = Token::lexer(&contents).spanned().collect::<Vec<_>>();
            tokens.data = lex.into_iter().chain(tokens.data).collect();

            continue;
        }
        if !in_comment {
            new_tokens.data.push((token, range));
        }
    }
    new_tokens
}
fn parse_statement(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.peek(0) {
        (Token::Print, _) => parse_print(tokens),
        (Token::Function, _) => parse_function_def(tokens),
        (Token::While, _) => parse_while(tokens),
        (Token::For, _) => parse_for_in(tokens),
        (Token::Return, _) => parse_return(tokens),
        (Token::If, _) => statize(parse_if(tokens)),
        (Token::ObjectName(n), _) => match tokens.peek(1) {
            (Token::Plus, _) | (Token::Minus, _) | (Token::Multiply, _) => match tokens.peek(2) {
                (Token::Equal, _) => parse_var_math_statement(tokens),
                (_, s) => show_error(
                    format!("Expected statement, found expression"),
                    s,
                    tokens.meta.clone(),
                ),
            },
            (Token::OpenParen, _) => statize(parse_function_call(tokens)),
            (Token::Equal, _) => parse_var_assignment(tokens),
            (_, s) => show_error(
                format!("Expected opening parenthesis"),
                s,
                tokens.meta.clone(),
            ),
        },
        (Token::Let, _) => parse_var_def(tokens),
        (_, s) => show_error(format!("Expected statement"), s, tokens.meta.clone()),
    }
}
fn statize(
    expression: (Expression, ParserStream<(Token, Range<usize>), String>),
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    (Statement::Expr(expression.0), expression.1)
}
fn parse_expression(
    tokens: ParserStream<(Token, Range<usize>), String>,
    greedy: bool,
) -> (Expression, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    let mut curr = match tokens.clone().peek(0) {
        (Token::Int(num), _) => {
            tokens.consume(0);
            (Expression::Int(*num), tokens)
        }
        (Token::StringLiteral(s), _) => {
            tokens.consume(0);
            (Expression::String(s.clone()), tokens)
        }
        (Token::True, _) => {
            tokens.consume(0);
            (Expression::Bool(true), tokens)
        }
        (Token::False, _) => {
            tokens.consume(0);
            (Expression::Bool(false), tokens)
        }
        (Token::Minus, _) => {
            tokens.consume(0);

            let expr = parse_expression(tokens, false);
            tokens = expr.1;
            (
                Expression::Minus(Box::new(Expression::Int(0)), Box::new(expr.0)),
                tokens,
            )
        }
        (Token::OpenParen, _) => {
            tokens.consume(0);
            let expr = parse_expression(tokens, true);
            tokens = expr.1;
            match tokens.consume(0) {
                (Token::CloseParen, _) => {}
                (_, s) => show_error(format!("Expected closing parenthesis"), &s, tokens.meta),
            };
            (expr.0, tokens)
        }
        (Token::OpenBracket, _) => {
            tokens.consume(0);
            let mut first = true;
            let mut items = vec![];
            while !tokens.empty() && tokens.peek(0).0 != Token::CloseBracket {
                if !first {
                    match tokens.consume(0) {
                        (Token::Comma, _) => {}
                        (_, s) => show_error(format!("Expected comma"), &s, tokens.meta),
                    };
                }
                first = false;
                let res = parse_expression(tokens, true);
                tokens = res.1;
                items.push(res.0);
            }
            match tokens.consume(0) {
                (Token::CloseBracket, _) => {}
                (_, s) => show_error(format!("Expected closing bracket"), &s, tokens.meta),
            };

            (Expression::Array(items), tokens)
        }

        (Token::ObjectName(n), _) => match tokens.clone().peek(1) {
            (Token::OpenParen, _) => parse_function_call(tokens),
            _ => {
                tokens.consume(0);
                (Expression::VariableReference(n.to_string()), tokens)
            }
        },
        (_, s) => show_error(format!("Expected expression"), &s, tokens.meta.clone()),
    };
    tokens = curr.1.clone();
    loop {
        if greedy {
            match tokens.peek(0) {
                (Token::DoubleEq, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::EqCmp(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::GreaterThan, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::GtCmp(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::Period, _) => {
                    tokens.consume(0);
                    let expr = parse_property(&mut tokens);
                    curr = (
                        Expression::PropertyAccess(Box::new(curr.0), Box::new(expr)),
                        tokens.clone(),
                    );
                }
                (Token::LessThan, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::LtCmp(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::GreaterThanEq, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::GteCmp(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::LessThanEq, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::LteCmp(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::Range, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::Range(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::Plus, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (Expression::Plus(Box::new(curr.0), Box::new(expr.0)), tokens);
                }
                (Token::Minus, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::Minus(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::OpenBracket, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    match tokens.consume(0) {
                        (Token::CloseBracket, _) => {}
                        (_, s) => show_error(format!("Expected closing bracket"), &s, tokens.meta),
                    };

                    return (
                        Expression::ArrayIndex(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                (Token::Multiply, _) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        Expression::Multiply(Box::new(curr.0), Box::new(expr.0)),
                        tokens,
                    );
                }
                _ => return curr,
            };
        } else {
            return curr;
        }
    }
}
fn parse_property(tokens: &mut ParserStream<(Token, Range<usize>), String>) -> Expression {
    match tokens.peek(0) {
        (Token::ObjectName(name), _) => match tokens.peek(1) {
            (Token::OpenParen, _) => {
                let res = parse_function_call(tokens.clone());
                *tokens = res.1;
                res.0
            }
            (_, s) => show_error(format!("Expected valid property"), &s, tokens.meta.clone()),
        },
        (_, s) => show_error(format!("Expected valid property"), &s, tokens.meta.clone()),
    }
}
fn parse_var_def(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::Let, _) => {}
        (_, s) => show_error(format!("Expected let keyword"), &s, tokens.meta),
    };

    match tokens.consume(0) {
        (Token::ObjectName(s), _) => {
            match tokens.consume(0) {
                (Token::Equal, _) => {}
                (_, s) => show_error(format!("Expected equal sign"), &s, tokens.meta),
            };
            let (val, t) = parse_expression(tokens, true);
            (Statement::VarDef(s, val), t)
        }
        (_, s) => show_error(format!("Expected variable name"), &s, tokens.meta),
    }
}
fn parse_function_def(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::Function, _) => {}
        (_, s) => show_error(format!("Expected function keyword"), &s, tokens.meta),
    };

    let func_name = match tokens.consume(0) {
        (Token::ObjectName(s), _) => s,
        (_, s) => show_error(format!("Expected function name"), &s, tokens.meta),
    };
    match tokens.consume(0) {
        (Token::OpenParen, _) => {}
        (_, s) => show_error(format!("Expected opening parenthesis"), &s, tokens.meta),
    };

    let mut args = vec![];
    let mut first_value = true;
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseParen {
        if !first_value {
            match tokens.consume(0) {
                (Token::Comma, _) => {}
                (_, s) => show_error(format!("Expected comma"), &s, tokens.meta),
            };
        }
        first_value = false;
        let arg_name = match tokens.consume(0) {
            (Token::ObjectName(s), _) => s,
            (_, s) => show_error(format!("Expected argument name"), &s, tokens.meta),
        };
        args.push(arg_name);
    }

    match tokens.consume(0) {
        (Token::CloseParen, _) => {}
        (_, s) => show_error(format!("Expected closing parenthesis"), &s, tokens.meta),
    };
    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s, tokens.meta),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    match tokens.consume(0) {
        (Token::CloseBrace, _) => {}
        (_, s) => show_error(format!("Expected closing brace"), &s, tokens.meta),
    };
    (Statement::FuncDef(func_name, args, statements), tokens)
}
fn parse_return(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::Return, _) => {}
        (_, s) => show_error(format!("Expected return keyword"), &s, tokens.meta),
    };

    let e = parse_expression(tokens, true);
    tokens = e.1;

    (Statement::Return(e.0), tokens)
}
fn parse_while(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::While, _) => {}
        (_, s) => show_error(format!("Expected while keyword"), &s, tokens.meta),
    };

    let condition = parse_expression(tokens, true);

    tokens = condition.1;

    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s, tokens.meta),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    match tokens.consume(0) {
        (Token::CloseBrace, _) => {}
        (_, s) => show_error(format!("Expected closing brace"), &s, tokens.meta),
    };
    (Statement::WhileLoop(condition.0, statements), tokens)
}
fn parse_for_in(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::For, _) => {}
        (_, s) => show_error(format!("Expected for keyword"), &s, tokens.meta),
    };

    let var_name = match tokens.consume(0) {
        (Token::ObjectName(n), _) => n,
        (_, s) => show_error(format!("Expected variable name"), &s, tokens.meta),
    };

    match tokens.consume(0) {
        (Token::In, _) => {}
        (_, s) => show_error(format!("Expected in keyword"), &s, tokens.meta),
    };

    let iter = parse_expression(tokens, true);

    tokens = iter.1;

    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s, tokens.meta),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    match tokens.consume(0) {
        (Token::CloseBrace, _) => {}
        (_, s) => show_error(format!("Expected closing brace"), &s, tokens.meta),
    };
    (Statement::ForIn(var_name, iter.0, statements), tokens)
}
fn parse_if(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Expression, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::If, _) => {}
        (_, s) => show_error(format!("Expected if keyword"), &s, tokens.meta),
    };

    let condition = parse_expression(tokens, true);

    tokens = condition.1;

    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s, tokens.meta),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    match tokens.consume(0) {
        (Token::CloseBrace, _) => {}
        (_, s) => show_error(format!("Expected closing brace"), &s, tokens.meta),
    };
    (Expression::If(Box::new(condition.0), statements), tokens)
}
fn parse_statement_outer(tokens: &mut ParserStream<(Token, Range<usize>), String>) -> Statement {
    let start_of_statement = tokens.peek(0).1.start;
    let (statement, stream) = parse_statement(tokens.clone());
    *tokens = stream;
    match tokens.peek(0) {
        (Token::EOL, _) => {
            tokens.consume(0);
        }
        (_, s) if requires_semicolon(statement.clone()) => {
            show_error(
                format!("Expected semicolon and newline"),
                &(start_of_statement..s.start),
                tokens.meta.clone(),
            );
        }
        _ => {}
    };
    statement
}
fn requires_semicolon(statement: Statement) -> bool {
    match statement {
        Statement::FuncDef(_, _, _) => false,
        Statement::WhileLoop(_, _) => false,
        Statement::ForIn(_, _, _) => false,
        Statement::Expr(Expression::If(_, _)) => false,
        _ => true,
    }
}
fn parse_var_assignment(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::ObjectName(s), _) => {
            match tokens.consume(0) {
                (Token::Equal, _) => {}
                (_, s) => show_error(format!("Expected equal sign"), &s, tokens.meta),
            };
            let (val, t) = parse_expression(tokens, true);
            (Statement::VarAssign(s, val), t)
        }
        (_, s) => show_error(format!("Expected variable name"), &s, tokens.meta),
    }
}
fn parse_var_math_statement(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::ObjectName(s), _) => {
            match tokens.consume(0) {
                (Token::Plus, _) => {}
                (_, s) => show_error(format!("Expected plus sign"), &s, tokens.meta),
            };

            match tokens.consume(0) {
                (Token::Equal, _) => {}
                (_, s) => show_error(format!("Expected equal sign"), &s, tokens.meta),
            };
            let (val, t) = parse_expression(tokens, true);
            (
                Statement::VarAssign(
                    s.clone(),
                    Expression::Plus(Box::new(Expression::VariableReference(s)), Box::new(val)),
                ),
                t,
            )
        }
        (_, s) => show_error(format!("Expected variable name"), &s, tokens.meta),
    }
}
fn parse_function_call(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Expression, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::ObjectName(n), _) => {
            match tokens.consume(0) {
                (Token::OpenParen, _) => {}
                (_, s) => show_error(format!("Expected opening parenthesis"), &s, tokens.meta),
            };

            let mut args = vec![];
            let mut first_value = true;
            while !tokens.empty() && tokens.peek(0).0 != Token::CloseParen {
                if !first_value {
                    match tokens.consume(0) {
                        (Token::Comma, _) => {}
                        (_, s) => show_error(format!("Expected comma"), &s, tokens.meta),
                    };
                }
                first_value = false;
                let expr = parse_expression(tokens, true);
                tokens = expr.1;
                args.push(expr.0);
            }
            match tokens.consume(0) {
                (Token::CloseParen, _) => {}
                (_, s) => show_error(format!("Expected closing parenthesis"), &s, tokens.meta),
            };
            (Expression::FuncCall(n, args), tokens)
        }
        (_, s) => show_error(format!("Expected valid identifier"), &s, tokens.meta),
    }
}
fn parse_print(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::Print, _) => {
            let expr = parse_expression(tokens, true);
            (Statement::Print(expr.0), expr.1)
        }
        (_, s) => show_error(format!("Expected print statement"), &s, tokens.meta),
    }
}

pub fn show_error(error: String, span: &Range<usize>, code: String) -> ! {
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::files::SimpleFiles;
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

    // `files::SimpleFile` and `files::SimpleFiles` help you get up and running with
    // `codespan-reporting` quickly! More complicated use cases can be supported
    // by creating custom implementations of the `files::Files` trait.

    let mut files = SimpleFiles::new();

    let file_id = files.add("", code);

    // We normally recommend creating a custom diagnostic data type for your
    // application, and then converting that to `codespan-reporting`'s diagnostic
    // type, but for the sake of this example we construct it directly.

    let diagnostic = Diagnostic::error()
        .with_message(error.clone())
        .with_labels(vec![
            Label::primary(file_id, span.clone()).with_message(error)
        ]);

    // We now set up the writer and configuration, and then finally render the
    // diagnostic to standard error.

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    panic!();
}
#[cfg(test)]
mod tests {
    use super::*;
    fn stream(contents: String) -> ParserStream<(Token, std::ops::Range<usize>), String> {
        let lex = Token::lexer(&contents).spanned().collect::<Vec<_>>();
        println!("{:#?}", Token::lexer(&contents).collect::<Vec<_>>());
        ParserStream::new(
            lex.clone(),
            (Token::Error, lex.last().unwrap().1.clone()),
            contents,
        )
    }
    #[test]
    fn add() {
        assert_eq!(
            parse_expression(stream(format!("5 + 2")), true).0,
            Expression::Plus(Box::new(Expression::Int(5)), Box::new(Expression::Int(2)))
        );
    }
    #[test]
    fn subtract() {
        assert_eq!(
            parse_expression(stream(format!("5 - 2")), true).0,
            Expression::Minus(Box::new(Expression::Int(5)), Box::new(Expression::Int(2)))
        );
    }
    #[test]
    fn multiply() {
        assert_eq!(
            parse_expression(stream(format!("5 * 2")), true).0,
            Expression::Multiply(Box::new(Expression::Int(5)), Box::new(Expression::Int(2)))
        );
    }
    #[test]
    fn negative_ordering() {
        assert_eq!(
            parse_expression(stream(format!("-5 + 2")), true).0,
            Expression::Plus(
                Box::new(Expression::Minus(
                    Box::new(Expression::Int(0)),
                    Box::new(Expression::Int(5))
                )),
                Box::new(Expression::Int(2))
            )
        );
    }
}
