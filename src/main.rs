mod value;

mod compiler;
mod parser;

fn main() {
    let err = parser::parse("(function main () int (mul 0i37 2i32))").unwrap_err();
    eprintln!("{}", err);
}
