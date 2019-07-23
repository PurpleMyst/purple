mod value;

mod compiler;
mod parser;

fn main() {
    let err = parser::parse("(function main () int 0i32)7").unwrap_err();
    eprintln!("{}", err);
}
