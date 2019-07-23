mod value;

mod compiler;
mod parser;

fn main() {
    let (rest, v) = parser::parse("(function main () int 0i32)").unwrap();
    assert_eq!(rest, "");
    println!("{:#?}", v);
    println!("{:#?}", compiler::Compiler::new("main").compile(v));
}
