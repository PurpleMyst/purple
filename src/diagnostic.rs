#![deny(unused_must_use)]

/// A code diagnostic, such as an error or warning
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// The span of this diagnostic
    pub span: (usize, usize),

    /// The level of this diagnostic and its color (e.g. "error", "warning")
    pub level: (&'static str, colorful::Color),

    /// The message attached to the diagnostic
    pub level_message: Option<String>,

    /// The message to be placed on the space above the code line
    pub below_message: Option<String>,

    /// The note to be placed after the message
    pub note: Option<String>,
}

fn offset_to_position(s: &str, offset: usize) -> (usize, usize) {
    s.chars().take(offset).fold((0, 0), |(row, col), c| {
        if c == '\n' {
            (row + 1, 0)
        } else {
            (row, col + 1)
        }
    })
}

macro_rules! builder_method {
    ($($name:ident: $ty:ty),*) => {
        $(
        #[allow(dead_code)]
        pub fn $name(self, $name: impl Into<$ty>) -> Self {
            assert!(self.$name.is_none());
            Self {
                $name: Some($name.into()),
                ..self
            }
        }
        )*
    }
}

impl Diagnostic {
    pub fn new(level: (&'static str, colorful::Color), span: (usize, usize)) -> Self {
        Self {
            level,
            span,
            below_message: None,
            level_message: None,
            note: None,
        }
    }

    builder_method!(level_message: String, below_message: String, note: String);

    pub fn show(&self, input: &str, filename: &str) {
        use colorful::{Color, Colorful};

        let (row, col) = offset_to_position(&input, self.span.0);

        let line = input.split("\n").nth(row).expect("line");

        let indent = (row + 1).to_string().len() + 1;

        // header
        print!("{}", self.level.0.color(self.level.1),);

        if let Some(ref level_message) = self.level_message {
            println!(": {}", level_message);
        } else {
            println!();
        }

        // source location
        println!(
            "{}{} {}:{}:{}",
            " ".repeat(indent - 1),
            "-->".color(Color::Blue),
            filename,
            row + 1,
            col + 1
        );

        // Above message, currently not supported cause I don't see the use for it
        println!("{}{}", " ".repeat(indent), "|".color(Color::Blue));

        // code line
        println!(
            "{} {} {}",
            (row + 1).to_string().color(Color::Blue),
            "|".color(Color::Blue),
            line
        );

        // bottom context
        print!(
            "{}{}{}{}",
            " ".repeat(indent),
            "|".color(Color::Blue),
            " ".repeat(col + 1),
            "^".repeat(self.span.1 - self.span.0).color(Color::Yellow)
        );

        if let Some(below_message) = &self.below_message {
            println!(" {}", (below_message as &str).color(Color::Yellow));
        } else {
            println!();
        }

        if let Some(note) = &self.note {
            println!(
                "{}{} {}: {}",
                " ".repeat(indent),
                "=".color(Color::Blue),
                "note".bold(),
                note
            );
        }
    }
}
