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

    #[token("null")]
    Null,

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

    #[token("!")]
    ExclamationPoint,

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

    #[token("%")]
    Percent,

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
pub struct ParserStream<T> {
    pub data: Vec<T>,
    pub default: T,
}
impl<T> ParserStream<T>
where
    T: Clone,
{
    pub fn new(data: Vec<T>, default: T) -> Self {
        Self { data, default }
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

pub type RangedT<T> = (T, TaggedRange);
pub type TaggedRange = (Range<usize>, (String, String));
type TokenStream = ParserStream<(Token, TaggedRange)>;

#[derive(Debug)]
pub struct Code {
    pub statements: Vec<RangedT<Statement>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Print(RangedT<Expression>),
    Expr(RangedT<Expression>),
    VarDef(String, RangedT<Expression>),
    VarAssign(String, RangedT<Expression>),
    FuncDef(String, Vec<String>, Vec<RangedT<Statement>>),
    WhileLoop(RangedT<Expression>, Vec<RangedT<Statement>>),
    ForIn(String, RangedT<Expression>, Vec<RangedT<Statement>>),
    Return(RangedT<Expression>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Int(i32),
    String(String),
    FuncCall(String, Vec<RangedT<Expression>>),
    VariableReference(String),
    Bool(bool),
    EqCmp(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    GtCmp(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    LtCmp(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    GteCmp(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    LteCmp(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    Plus(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    Minus(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    Multiply(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    Mod(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    Range(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    If(Box<RangedT<Expression>>, Vec<RangedT<Statement>>),
    Array(Vec<RangedT<Expression>>),
    ArrayIndex(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    PropertyAccess(Box<RangedT<Expression>>, Box<RangedT<Expression>>),
    Null,
}

pub fn parse_code(tokens: TokenStream) -> Code {
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
fn preprocess(tokens: TokenStream) -> TokenStream {
    let mut new_tokens = ParserStream::new(vec![], tokens.default.clone());
    let mut tokens = tokens;
    let mut in_comment = false;
    while !tokens.data.is_empty() {
        let (token, range) = tokens.consume(0);
        if token == Token::Newline || (token == Token::EOL && in_comment) {
            in_comment = false;
            continue;
        }
        if token == Token::Comment {
            in_comment = true;
            continue;
        }
        if token == Token::Use {
            let (path, file_location_span) = match tokens.consume(0) {
                (Token::StringLiteral(path), s) => (path, s),
                (_, s) => show_error(format!("Expected path to elk file"), &s),
            };
            let mut file = match File::open(path.clone()) {
                Ok(f) => f,
                Err(e) => show_error(format!("{}", e), &file_location_span),
            };
            let mut contents = String::new();
            match file.read_to_string(&mut contents) {
                Ok(_) => {}
                Err(e) => show_error(format!("{}", e), &file_location_span),
            }
            if !contents.ends_with("\n") {
                contents += "\n";
            }
            match tokens.consume(0) {
                (Token::EOL, _) => {}
                (_, s) => {
                    show_error(format!("Expected semicolon and newline"), &s);
                }
            };
            let lex = Token::lexer(&contents)
                .spanned()
                .map(|n| (n.0, (n.1, (path.clone(), contents.clone()))))
                .collect::<Vec<_>>();

            tokens.data = lex.into_iter().chain(tokens.data).collect();

            continue;
        }
        if !in_comment {
            new_tokens.data.push((token, range));
        }
    }
    new_tokens
}
fn parse_statement(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
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
                (_, s) => show_error(format!("Expected statement, found expression"), s),
            },
            (Token::OpenParen, _) => statize(parse_function_call(tokens)),
            (Token::Equal, _) => parse_var_assignment(tokens),
            (_, s) => show_error(format!("Expected opening parenthesis"), s),
        },
        (Token::Let, _) => parse_var_def(tokens),
        (_, s) => show_error(format!("Expected statement"), s),
    }
}
fn statize(expression: (RangedT<Expression>, TokenStream)) -> (RangedT<Statement>, TokenStream) {
    (
        (Statement::Expr(expression.clone().0), expression.0 .1),
        expression.1,
    )
}
fn parse_expression(tokens: TokenStream, greedy: bool) -> (RangedT<Expression>, TokenStream) {
    let mut tokens = tokens;
    let mut curr = match tokens.clone().peek(0) {
        (Token::Int(num), s) => {
            tokens.consume(0);
            ((Expression::Int(*num), s.clone()), tokens)
        }
        (Token::StringLiteral(s), span) => {
            tokens.consume(0);
            ((Expression::String(s.clone()), span.clone()), tokens)
        }
        (Token::True, s) => {
            tokens.consume(0);
            ((Expression::Bool(true), s.clone()), tokens)
        }
        (Token::False, s) => {
            tokens.consume(0);
            ((Expression::Bool(false), s.clone()), tokens)
        }
        (Token::Null, s) => {
            tokens.consume(0);
            ((Expression::Null, s.clone()), tokens)
        }
        (Token::Minus, s) => {
            tokens.consume(0);

            let expr = parse_expression(tokens, false);
            tokens = expr.clone().1;
            (
                (
                    Expression::Minus(
                        Box::new((Expression::Int(0), expr.clone().0 .1)),
                        Box::new(expr.0),
                    ),
                    s.clone(),
                ),
                tokens,
            )
        }
        (Token::ExclamationPoint, s) => {
            tokens.consume(0);

            let expr = parse_expression(tokens, false);
            tokens = expr.clone().1;
            (
                (
                    Expression::EqCmp(
                        Box::new(expr.clone().0),
                        Box::new((Expression::Bool(false), expr.0 .1)),
                    ),
                    s.clone(),
                ),
                tokens,
            )
        }
        (Token::OpenParen, _) => {
            tokens.consume(0);
            let expr = parse_expression(tokens, true);
            tokens = expr.1;
            match tokens.consume(0) {
                (Token::CloseParen, _) => {}
                (_, s) => show_error(format!("Expected closing parenthesis"), &s),
            };
            ((expr.0 .0, expr.0 .1), tokens)
        }
        (Token::OpenBracket, start) => {
            tokens.consume(0);
            let mut first = true;
            let mut items = vec![];
            while !tokens.empty() && tokens.peek(0).0 != Token::CloseBracket {
                if !first {
                    match tokens.consume(0) {
                        (Token::Comma, _) => {}
                        (_, s) => show_error(format!("Expected comma"), &s),
                    };
                }
                first = false;
                let res = parse_expression(tokens, true);
                tokens = res.1;
                items.push(res.0);
            }
            let end = match tokens.consume(0) {
                (Token::CloseBracket, e) => e.0,
                (_, s) => show_error(format!("Expected closing bracket"), &s),
            };

            (
                (
                    Expression::Array(items),
                    (start.0.start..end.end, start.1.clone()),
                ),
                tokens,
            )
        }

        (Token::ObjectName(n), s) => match tokens.clone().peek(1) {
            (Token::OpenParen, _) => parse_function_call(tokens),
            _ => {
                tokens.consume(0);
                (
                    (Expression::VariableReference(n.to_string()), s.clone()),
                    tokens,
                )
            }
        },
        (_, s) => show_error(format!("Expected expression"), &s),
    };
    tokens = curr.1.clone();
    loop {
        if greedy {
            match tokens.peek(0).clone() {
                (Token::DoubleEq, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::EqCmp(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::GreaterThan, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::GtCmp(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::Period, s) => {
                    tokens.consume(0);
                    let expr = parse_property(&mut tokens);
                    curr = (
                        (
                            Expression::PropertyAccess(Box::new(curr.0), Box::new(expr)),
                            s.clone(),
                        ),
                        tokens.clone(),
                    );
                }
                (Token::LessThan, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::LtCmp(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::GreaterThanEq, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::GteCmp(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::LessThanEq, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::LteCmp(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::Range, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::Range(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::Plus, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::Plus(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::Minus, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::Minus(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::OpenBracket, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    match tokens.consume(0) {
                        (Token::CloseBracket, _) => {}
                        (_, s) => show_error(format!("Expected closing bracket"), &s),
                    };

                    return (
                        (
                            Expression::ArrayIndex(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::Multiply, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::Multiply(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
                        tokens,
                    );
                }
                (Token::Percent, s) => {
                    tokens.consume(0);
                    let expr = parse_expression(tokens, true);
                    tokens = expr.1;
                    return (
                        (
                            Expression::Mod(Box::new(curr.0), Box::new(expr.0)),
                            s.clone(),
                        ),
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
fn parse_property(tokens: &mut TokenStream) -> RangedT<Expression> {
    match tokens.peek(0) {
        (Token::ObjectName(name), _) => match tokens.peek(1) {
            (Token::OpenParen, _) => {
                let res = parse_function_call(tokens.clone());
                *tokens = res.1;
                res.0
            }
            (_, s) => show_error(format!("Expected valid property"), &s),
        },
        (_, s) => show_error(format!("Expected valid property"), &s),
    }
}
fn parse_var_def(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    let start = match tokens.consume(0) {
        (Token::Let, s) => s,
        (_, s) => show_error(format!("Expected let keyword"), &s),
    };

    match tokens.consume(0) {
        (Token::ObjectName(s), _) => {
            match tokens.consume(0) {
                (Token::Equal, _) => {}
                (_, s) => show_error(format!("Expected equal sign"), &s),
            };
            let (val, t) = parse_expression(tokens, true);
            (
                (
                    Statement::VarDef(s, val.clone()),
                    (start.0.start..val.1 .0.end, start.1),
                ),
                t,
            )
        }
        (_, s) => show_error(format!("Expected variable name"), &s),
    }
}
fn parse_function_def(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    let start = match tokens.consume(0) {
        (Token::Function, s) => s,
        (_, s) => show_error(format!("Expected function keyword"), &s),
    };

    let func_name = match tokens.consume(0) {
        (Token::ObjectName(s), _) => s,
        (_, s) => show_error(format!("Expected function name"), &s),
    };
    match tokens.consume(0) {
        (Token::OpenParen, _) => {}
        (_, s) => show_error(format!("Expected opening parenthesis"), &s),
    };

    let mut args = vec![];
    let mut first_value = true;
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseParen {
        if !first_value {
            match tokens.consume(0) {
                (Token::Comma, _) => {}
                (_, s) => show_error(format!("Expected comma"), &s),
            };
        }
        first_value = false;
        let arg_name = match tokens.consume(0) {
            (Token::ObjectName(s), _) => s,
            (_, s) => show_error(format!("Expected argument name"), &s),
        };
        args.push(arg_name);
    }

    match tokens.consume(0) {
        (Token::CloseParen, _) => {}
        (_, s) => show_error(format!("Expected closing parenthesis"), &s),
    };
    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    let end = match tokens.consume(0) {
        (Token::CloseBrace, e) => e,
        (_, s) => show_error(format!("Expected closing brace"), &s),
    };
    (
        (
            Statement::FuncDef(func_name, args, statements),
            (start.0.start..end.0.end, end.1),
        ),
        tokens,
    )
}
fn parse_return(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    let start = match tokens.consume(0) {
        (Token::Return, s) => s,
        (_, s) => show_error(format!("Expected return keyword"), &s),
    };

    let e = parse_expression(tokens, true);
    tokens = e.clone().1;

    (
        (
            Statement::Return(e.clone().0),
            (start.0.start..e.0 .1 .0.end, start.1),
        ),
        tokens,
    )
}
fn parse_while(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    let start = match tokens.consume(0) {
        (Token::While, s) => s,
        (_, s) => show_error(format!("Expected while keyword"), &s),
    };

    let condition = parse_expression(tokens, true);

    tokens = condition.1;

    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    let end = match tokens.consume(0) {
        (Token::CloseBrace, e) => e,
        (_, s) => show_error(format!("Expected closing brace"), &s),
    };
    (
        (
            Statement::WhileLoop(condition.0, statements),
            (start.0.start..end.0.end, end.1),
        ),
        tokens,
    )
}
fn parse_for_in(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    let start = match tokens.consume(0) {
        (Token::For, s) => s,
        (_, s) => show_error(format!("Expected for keyword"), &s),
    };

    let var_name = match tokens.consume(0) {
        (Token::ObjectName(n), _) => n,
        (_, s) => show_error(format!("Expected variable name"), &s),
    };

    match tokens.consume(0) {
        (Token::In, _) => {}
        (_, s) => show_error(format!("Expected in keyword"), &s),
    };

    let iter = parse_expression(tokens, true);

    tokens = iter.1;

    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    let end = match tokens.consume(0) {
        (Token::CloseBrace, s) => s,
        (_, s) => show_error(format!("Expected closing brace"), &s),
    };
    (
        (
            Statement::ForIn(var_name, iter.0, statements),
            (start.0.start..end.0.end, start.1),
        ),
        tokens,
    )
}
fn parse_if(tokens: TokenStream) -> (RangedT<Expression>, TokenStream) {
    let mut tokens = tokens;
    let start = match tokens.consume(0) {
        (Token::If, s) => s,
        (_, s) => show_error(format!("Expected if keyword"), &s),
    };

    let condition = parse_expression(tokens, true);

    tokens = condition.1;

    match tokens.consume(0) {
        (Token::OpenBrace, _) => {}
        (_, s) => show_error(format!("Expected opening brace"), &s),
    };
    let mut statements = vec![];
    while !tokens.empty() && tokens.peek(0).0 != Token::CloseBrace {
        statements.push(parse_statement_outer(&mut tokens));
    }

    let end = match tokens.consume(0) {
        (Token::CloseBrace, s) => s,
        (_, s) => show_error(format!("Expected closing brace"), &s),
    };
    (
        (
            Expression::If(Box::new(condition.0), statements),
            (start.0.start..end.0.end, start.1),
        ),
        tokens,
    )
}
fn parse_statement_outer(tokens: &mut TokenStream) -> RangedT<Statement> {
    let start_of_statement = tokens.peek(0).1 .0.start;
    let (statement, stream) = parse_statement(tokens.clone());
    *tokens = stream;
    match tokens.peek(0) {
        (Token::EOL, _) => {
            tokens.consume(0);
        }
        (_, s) if requires_semicolon(statement.clone().0) => {
            show_error(
                format!("Expected semicolon and newline"),
                &(start_of_statement..s.0.start, s.1.clone()),
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
        Statement::Expr((Expression::If(_, _), _)) => false,
        _ => true,
    }
}
fn parse_var_assignment(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::ObjectName(s), start) => {
            match tokens.consume(0) {
                (Token::Equal, _) => {}
                (_, s) => show_error(format!("Expected equal sign"), &s),
            };
            let (val, t) = parse_expression(tokens, true);
            (
                (
                    Statement::VarAssign(s, val.clone()),
                    (start.0.start..val.1 .0.end, start.1),
                ),
                t,
            )
        }
        (_, s) => show_error(format!("Expected variable name"), &s),
    }
}
fn parse_var_math_statement(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::ObjectName(s), start) => {
            match tokens.consume(0) {
                (Token::Plus, _) => {}
                (_, s) => show_error(format!("Expected plus sign"), &s),
            };

            match tokens.consume(0) {
                (Token::Equal, _) => {}
                (_, s) => show_error(format!("Expected equal sign"), &s),
            };
            let (val, t) = parse_expression(tokens, true);
            (
                (
                    Statement::VarAssign(
                        s.clone(),
                        (
                            Expression::Plus(
                                Box::new((
                                    Expression::VariableReference(s),
                                    (start.clone().0, start.clone().1),
                                )),
                                Box::new(val.clone()),
                            ),
                            (start.0.start..val.1 .0.end, start.clone().1),
                        ),
                    ),
                    (start.0.start..val.1 .0.end, start.1),
                ),
                t,
            )
        }
        (_, s) => show_error(format!("Expected variable name"), &s),
    }
}
fn parse_function_call(tokens: TokenStream) -> (RangedT<Expression>, TokenStream) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::ObjectName(n), start) => {
            match tokens.consume(0) {
                (Token::OpenParen, _) => {}
                (_, s) => show_error(format!("Expected opening parenthesis"), &s),
            };

            let mut args = vec![];
            let mut first_value = true;
            while !tokens.empty() && tokens.peek(0).0 != Token::CloseParen {
                if !first_value {
                    match tokens.consume(0) {
                        (Token::Comma, _) => {}
                        (_, s) => show_error(format!("Expected comma"), &s),
                    };
                }
                first_value = false;
                let expr = parse_expression(tokens, true);
                tokens = expr.1;
                args.push(expr.0);
            }
            let end = match tokens.consume(0) {
                (Token::CloseParen, e) => e,
                (_, s) => show_error(format!("Expected closing parenthesis"), &s),
            };
            (
                (
                    Expression::FuncCall(n, args),
                    (start.0.start..end.0.end, start.1),
                ),
                tokens,
            )
        }
        (_, s) => show_error(format!("Expected valid identifier"), &s),
    }
}
fn parse_print(tokens: TokenStream) -> (RangedT<Statement>, TokenStream) {
    let mut tokens = tokens;
    match tokens.consume(0) {
        (Token::Print, start) => {
            let expr = parse_expression(tokens, true);
            (
                (
                    Statement::Print(expr.clone().0),
                    (start.0.start..expr.0 .1 .0.end, start.1),
                ),
                expr.1,
            )
        }
        (_, s) => show_error(format!("Expected print statement"), &s),
    }
}

pub fn show_error(error: String, span: &(Range<usize>, (String, String))) -> ! {
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::files::SimpleFiles;
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

    // `files::SimpleFile` and `files::SimpleFiles` help you get up and running with
    // `codespan-reporting` quickly! More complicated use cases can be supported
    // by creating custom implementations of the `files::Files` trait.

    let mut files = SimpleFiles::new();

    let file_id = files.add(span.1 .0.clone(), span.1 .1.clone());

    // We normally recommend creating a custom diagnostic data type for your
    // application, and then converting that to `codespan-reporting`'s diagnostic
    // type, but for the sake of this example we construct it directly.

    let diagnostic = Diagnostic::error()
        .with_message(error.clone())
        .with_labels(vec![
            Label::primary(file_id, span.0.clone()).with_message(error)
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
    fn stream(contents: String) -> ParserStream<(Token, (std::ops::Range<usize>, String))> {
        let lex = Token::lexer(&contents)
            .spanned()
            .map(|n| (n.0, (n.1, contents.clone())))
            .collect::<Vec<_>>();
        println!("{:#?}", Token::lexer(&contents).collect::<Vec<_>>());
        ParserStream::new(lex.clone(), (Token::Error, lex.last().unwrap().1.clone()))
    }
}
