mod diagnostic;
mod value;

mod compiler;
mod parser;

fn main() {
    match parser::parse("playground.purple") {
        Ok(v) => println!("{:?}", v),
        Err(errs) => errs.into_iter().for_each(|err| eprintln!("{}", err)),
    }
}
