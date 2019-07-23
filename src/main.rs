mod value;

mod compiler;
mod parser;

fn main() {
    println!("{:#?}", parser::parse("'(list 1 2 3)"));
}
