mod parser;
use parser::*;
mod interpreter;
use colour::*;
use logos::Logos;
use std::env;
use std::io::Read;
use std::{fs::File, ops::Range};

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(args.last().unwrap()).expect("Unable to open the file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Unable to read the file");
    if !contents.ends_with("\n") {
        contents += "\n";
    }
    let lex = Token::lexer(&contents)
        .spanned()
        .map(|n| (n.0, (n.1, (args.last().unwrap().clone(), contents.clone()))))
        .collect::<Vec<_>>();
    println!("{:#?}", Token::lexer(&contents).collect::<Vec<_>>());
    let code = parse_code(ParserStream::new(
        lex.clone(),
        (Token::Error, lex.last().unwrap().1.clone()),
    ));
    //println!("{:#?}", code);
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
