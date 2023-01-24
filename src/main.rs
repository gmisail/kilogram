use std::{fs::File, io::Read, time::Instant};
use owo_colors::OwoColorize;

use parser::parse;
use typechecker::typechecker::Typechecker;

pub mod ast;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod typechecker;

fn main() {
    let mut file = match File::open("./syntax/primary.kg") {
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
    println!("{}", match checker.resolve_type(tree) {
        Ok(_) => format!("{} Successful!", "[typechecker]".blue()),
        Err(error) => format!("{} {}", "[typechecker]".red(), error)
    })
}
