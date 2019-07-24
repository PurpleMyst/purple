mod diagnostic;
mod value;

mod compiler;
mod parser;

fn main() {
    let filename = "playground.purple";
    let input = std::fs::read_to_string(filename).unwrap();

    let mut compiler = compiler::Compiler::new("main");

    let result = parser::parse(&input).and_then(|value| {
        compiler
            .compile(value)
            .map_err(|error| vec![diagnostic::Diagnostic::from(error)])
    });

    match result {
        Ok(value) => println!("{:#?}", value),
        Err(diagnostics) => diagnostics
            .into_iter()
            .for_each(|diagnostic| diagnostic.show(&input, filename)),
    }
}
