use owo_colors::OwoColorize;
use std::{fs::File, io::Read};

pub mod ast;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod typechecker;
pub mod compiler;

use parser::parse;
use typechecker::Typechecker;
use compiler::Compiler;

fn main() {
    let mut file = match File::open("./syntax/hello_world.kg") {
        Err(e) => panic!("couldn't open file: {}", e),
        Ok(file) => file,
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read: {}", why),
        Ok(_) => println!("opened file",),
    }

    let tree = parse(s).unwrap();
    let mut checker = Typechecker::new();

    // for each expression...
    println!(
        "{}",
        match checker.resolve_type(&tree) {
            Ok(_) => format!("{} Successful!", "[typechecker]".blue()),
            Err(error) => format!("{} {}", "[typechecker]".red(), error),
        }
    );

    let compiler = Compiler::new(); 
    println!("{}", compiler.compile_expression(&tree));
}
