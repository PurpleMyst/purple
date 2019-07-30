// XXX: Can we unify these macros?
macro_rules! variant {
    ($var:expr => $variant:ident) => {{
        let var = $var;
        let span = var.span;
        if let $crate::value::ValueData::$variant(x) = var.data {
            Ok(x)
        } else {
            Err(Diagnostic::new(("error", colorful::Color::Red), span)
                .level_message(concat!("Expected value of type ", stringify!($variant))))
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
                .level_message(concat!("Expected value of type ", stringify!($variant))))
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
                .level_message(concat!("Expected value of type ", stringify!($variant))))
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

    let result = parser::parse(&input).and_then(|mut value| {
        typechecker::typecheck(&mut value).map_err(|error| vec![error])?;

        compiler::compile("main", value).map_err(|error| vec![error])
    });

    match result {
        Ok(s) => println!("{}", s),
        Err(diagnostics) => diagnostics
            .into_iter()
            .for_each(|diagnostic| diagnostic.show(&input, filename)),
    }
}
