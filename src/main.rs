use owo_colors::OwoColorize;
use std::{fs::File, io::Read};

pub mod ast;
pub mod compiler;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod typechecker;

use compiler::Compiler;
use parser::parse;
use typechecker::Typechecker;

fn main() {
    let mut file = match File::open("./syntax/hello_world.kg") {
        Err(e) => panic!("couldn't open file: {}", e),
        Ok(file) => file,
    };

    let mut s = String::new();
    println!(
        "{}",
        match file.read_to_string(&mut s) {
            Ok(_) => format!("{} Successful.", "[file]".blue()),
            Err(error) => format!("{} {}", "[file]".red(), error),
        }
    );

    let tree = match parse(s) {
        Ok(root) => {
            println!("{}", format!("{} Successful.", "[parser]".blue()));

            root
        }
        Err(error) => panic!("{} {}", "[parser]".red(), error),
    };

    let mut checker = Typechecker::new();

    // for each expression...
    println!(
        "{}",
        match checker.resolve_type(&tree) {
            Ok(_) => format!("{} Successful.", "[typechecker]".blue()),
            Err(error) => format!("{} {}", "[typechecker]".red(), error),
        }
    );

    let mut compiler = Compiler::new(checker.records);
    println!("{}", compiler.compile(&tree));
}
