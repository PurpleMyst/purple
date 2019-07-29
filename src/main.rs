// XXX: Can we unify `variant!` and `variant_ref!`?
macro_rules! variant {
    ($var:expr => $variant:ident) => {{
        let var = $var;
        let span = var.span;
        if let $crate::value::ValueData::$variant(x) = var.data {
            Ok(x)
        } else {
            Err(Diagnostic::new(("error", colorful::Color::Red), span)
                .level_message(concat!("Expected value of type ", stringify!($variant)).to_owned()))
        }
    }};
}

macro_rules! variant_ref {
    ($var:expr => $variant:ident) => {{
        let var = $var;
        let span = var.span;
        if let $crate::value::ValueData::$variant(ref x) = var.data {
            Ok(x)
        } else {
            Err(Diagnostic::new(("error", colorful::Color::Red), span)
                .level_message(concat!("Expected value of type ", stringify!($variant)).to_owned()))
        }
    }};
}

macro_rules! variant_ref_mut {
    ($var:expr => $variant:ident) => {{
        let var = $var;
        let span = var.span;
        if let $crate::value::ValueData::$variant(ref mut x) = var.data {
            Ok(x)
        } else {
            Err(Diagnostic::new(("error", colorful::Color::Red), span)
                .level_message(concat!("Expected value of type ", stringify!($variant)).to_owned()))
        }
    }};
}

mod diagnostic;
mod value;

mod compiler;
mod parser;
mod typechecker;

fn main() {
    let filename = "playground.purple";
    let input = std::fs::read_to_string(filename).unwrap();

    let mut compiler = compiler::Compiler::new("main");

    let result = parser::parse(&input).and_then(|mut value| {
        typechecker::Typechecker::new()
            .typecheck(&mut value)
            .map_err(|error| vec![error])?;

        compiler.compile(value).map_err(|error| vec![error])
    });

    match result {
        Ok(value) => println!("{:#?}", value),
        Err(diagnostics) => diagnostics
            .into_iter()
            .for_each(|diagnostic| diagnostic.show(&input, filename)),
    }
}
