use std::{fs::File, io::Read};

pub mod scanner;
pub mod token;

fn main() {
    let mut file = match File::open("./syntax/lex.kg") {
        Err(e) => panic!("couldn't open file: {}", e),
        Ok(file) => file,
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read: {}", why),
        Ok(_) => println!("opened file",),
    }

    let tokens = match scanner::scan(s) {
        Ok(t) => t,
        Err(e) => panic!("[err] {}", e),
    };

    for tok in tokens {
        println!(
            "line {} ({}...{}): {}",
            tok.line,
            tok.relative,
            tok.relative + tok.length,
            tok.kind
        );
    }
}
