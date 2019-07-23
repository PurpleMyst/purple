mod value;

mod compiler;
mod parser;

fn main() {
    let err = parser::parse("(function main () ò int 0i37)7").unwrap_err();
    eprintln!("{}", err);
}
