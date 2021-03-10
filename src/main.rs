use codespan_reporting::term;
use logos::Logos;
use std::io::prelude::*;
use std::{fs::File, ops::Range};
mod interpreter;
use colour::*;

#[derive(Logos, Debug, PartialEq, Clone)]
enum Token {
    #[token("__print")]
    Print,

    #[token("function ")]
    Function,

    #[token("let ")]
    Let,

    #[token(";\n")]
    EOL,

    #[token("=")]
    Equal,

    #[token(",")]
    Comma,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let slice = lex.slice();
        let n = slice[1..slice.len() - 1].to_string(); // Remove quotes
        Some(n)
    })]
    StringLiteral(String),

    #[regex("[0-9]+", |lex| lex.slice().to_string().parse::<i32>().expect("Failed to parse int literal"))]
    Int(i32),

    #[regex("[a-zA-Z_]+", |v| v.slice().to_string())]
    ObjectName(String),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

struct ParserStream<T, A> {
    data: Vec<T>,
    default: T,
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

fn main() {
    let mut file = File::open("code.elk").expect("Unable to open the file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Unable to read the file");
    if !contents.ends_with("\n") {
        contents += "\n";
    }
    let lex = Token::lexer(&contents).spanned().collect::<Vec<_>>();
    println!("{:#?}", Token::lexer(&contents).collect::<Vec<_>>());
    let code = parse_code(ParserStream::new(
        lex.clone(),
        (Token::Error, lex.last().unwrap().1.clone()),
        contents,
    ));
    println!("{:#?}", code);
    green_ln!("Parsed successfully!");
    let vars = interpreter::interpret_pub(code);
    green_ln!("Finished running");
    if vars.len() > 0 {
        blue_ln!("Var Dump:")
    }
    for var in vars {
        blue_ln!("{}", var.1);
    }
}

#[derive(Debug)]
enum AST {}

#[derive(Debug)]
pub struct Code {
    statements: Vec<Statement>,
}
#[derive(Debug, Clone)]
enum Statement {
    Print(Expression),
    Expr(Expression),
    VarDef(String, Expression),
    VarAssign(String, Expression),
    FuncDef(String, Vec<String>, Vec<Statement>),
}
#[derive(Debug, Clone)]
enum Expression {
    Int(i32),
    String(String),
    FuncCall(String, Vec<Expression>),
    VariableReference(String),
}

fn parse_code(tokens: ParserStream<(Token, Range<usize>), String>) -> Code {
    let mut tokens = tokens;
    let mut code = Code { statements: vec![] };
    while !tokens.empty() {
        let start_of_statement = tokens.peek(0).1.start;
        let (statement, stream) = parse_statement(tokens);
        code.statements.push(statement.clone());
        tokens = stream;
        match tokens.peek(0) {
            (Token::EOL, _) => {
                tokens.consume(0);
            }
            (_, s)
                if match statement {
                    Statement::FuncDef(_, _, _) => false,
                    _ => true,
                } =>
            {
                show_error(
                    format!("Expected semicolon and newline"),
                    &(start_of_statement..s.start),
                    tokens.meta,
                );
            }
            _ => {}
        };
    }
    return code;
}
fn parse_statement(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Statement, ParserStream<(Token, Range<usize>), String>) {
    let tokens = tokens;
    match tokens.peek(0) {
        (Token::Print, _) => parse_print(tokens),
        (Token::Function, _) => parse_function_def(tokens),
        (Token::ObjectName(n), _) => match tokens.peek(1) {
            (Token::OpenParen, _) => {
                let fc = parse_function_call(tokens);
                (Statement::Expr(fc.0), fc.1)
            }
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

fn parse_expression(
    tokens: ParserStream<(Token, Range<usize>), String>,
) -> (Expression, ParserStream<(Token, Range<usize>), String>) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::Int(num), _) => (Expression::Int(num), tokens),
        (Token::StringLiteral(s), _) => (Expression::String(s), tokens),
        (Token::ObjectName(n), _) => match tokens.peek(1) {
            (Token::OpenParen, _) => parse_function_call(tokens),
            _ => (Expression::VariableReference(n), tokens),
        },
        (_, s) => show_error(format!("Expected expression"), &s, tokens.meta),
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
            let (val, t) = parse_expression(tokens);
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
        let start_of_statement = tokens.peek(0).1.start;
        let (statement, stream) = parse_statement(tokens);
        statements.push(statement.clone());
        tokens = stream;
        match tokens.peek(0) {
            (Token::EOL, _) => {
                tokens.consume(0);
            }
            (_, s)
                if match statement {
                    Statement::FuncDef(_, _, _) => false,
                    _ => true,
                } =>
            {
                show_error(
                    format!("Expected semicolon and newline"),
                    &(start_of_statement..s.start),
                    tokens.meta,
                );
            }
            _ => {}
        };
    }

    match tokens.consume(0) {
        (Token::CloseBrace, _) => {}
        (_, s) => show_error(format!("Expected closing brace"), &s, tokens.meta),
    };
    (Statement::FuncDef(func_name, args, statements), tokens)
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
            let (val, t) = parse_expression(tokens);
            (Statement::VarAssign(s, val), t)
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
                let expr = parse_expression(tokens);
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
            let expr = parse_expression(tokens);
            (Statement::Print(expr.0), expr.1)
        }
        (_, s) => show_error(format!("Expected print statement"), &s, tokens.meta),
    }
}

fn show_error(error: String, span: &Range<usize>, code: String) -> ! {
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
