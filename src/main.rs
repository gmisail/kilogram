use owo_colors::OwoColorize;
use std::{
    fs::{self, File},
    io::Read,
    time::Instant,
};

pub mod ast;
pub mod compiler;
pub mod fresh;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod typechecker;

pub mod postprocess;
pub mod preprocess;

use compiler::Compiler;
use parser::parse;
use typechecker::Typechecker;

use crate::postprocess::apply_all;

fn compile(file: &str) -> Result<(), String> {
    let mut file = File::open(file).expect("Failed to load file.");

    let start_lex = Instant::now();
    let mut s = String::new();
    println!(
        "{}",
        match file.read_to_string(&mut s) {
            Ok(_) => format!("{} Successful.", "[file]".blue()),
            Err(error) => format!("{} {error}", "[file]".red()),
        }
    );
    println!("Finished lexing in {:?}", start_lex.elapsed());

    let start_parse = Instant::now();
    let tree = parse(s)?;
    println!("Finished parsing in {:?}", start_parse.elapsed());

    let start_type = Instant::now();
    let mut checker = Typechecker::new();
    let (_, root_node) = checker.resolve_type(&tree)?;
    println!("Finished type-checking in {:?}", start_type.elapsed());

    let start_post = Instant::now();
    let postprocessed_node = apply_all(&root_node, &checker.enums);
    println!("Finished postprocessing in {:?}", start_post.elapsed());

    let start_comp = Instant::now();
    let mut compiler = Compiler::new(checker);
    let source = compiler.compile(&postprocessed_node);
    println!("Finished compiling in {:?}", start_comp.elapsed());

    fs::write("./out.c", source).expect("Failed to write file.");

    Ok(())
}

fn main() {
    match compile("./syntax/generic.kg") {
        Ok(()) => println!("Done."),
        Err(e) => panic!("{e}"),
    }
}
