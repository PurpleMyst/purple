mod value;

mod compiler;
mod parser;

fn main() {
    match parser::parse("playground.purple") {
        Ok(v) => println!("{:?}", v),
        Err(err) => eprintln!("{}", err),
    }
}
