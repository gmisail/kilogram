use std::{fs::File, io::Read};

use parser::parse;

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

    typechecker::expr_type::from_literal(tree);
}
