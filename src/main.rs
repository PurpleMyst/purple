mod value;

mod compiler;
mod parser;

fn main() {
    match parser::parse("(function main () int (mul 0i42 2i69))") {
        Ok(v) => println!("{:?}", v),
        Err(err) => eprintln!("{}", err),
    }
}
