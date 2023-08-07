use std::{
    fs::{self, File},
    io::Read,
};
use tracing::error;

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

fn setup_tracing() {
    tracing_subscriber::fmt::init();
}

fn compile(file: &str) -> Result<(), String> {
    setup_tracing();

    let mut file = File::open(file).expect("Failed to load file.");

    let mut s = String::new();

    match file.read_to_string(&mut s) {
        Ok(_) => (),
        Err(e) => {
            error!("failed to read file: {e}");
            panic!()
        }
    };

    let tree = parse(s)?;

    let preprocessed_node = preprocess::apply_all(&tree);

    let mut checker = Typechecker::new();

    let (_, root_node) = checker.resolve_type(&preprocessed_node)?;

    let postprocessed_node = postprocess::apply_all(&root_node, &checker.enums);

    let mut compiler = Compiler::new(checker);

    let source = compiler.compile(&postprocessed_node);

    fs::write("./out.c", source).expect("Failed to write file.");

    Ok(())
}

fn main() {
    match compile("./syntax/generic.kg") {
        Ok(()) => println!("Done."),
        Err(e) => panic!("{e}"),
    }
}
