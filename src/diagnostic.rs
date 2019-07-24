#![deny(unused_must_use)]

/// A code diagnostic, such as an error or warning
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// The input this diagnostic refers to
    pub input: String,

    pub filename: String,

    /// The character where this diagnostic starts, inclusive
    pub start: usize,

    /// The character where this diagnostic ends, exclusive
    pub end: usize,

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

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colorful::{Color, Colorful};

        let (row, col) = offset_to_position(&self.input, self.start);

        let line = self.input.split("\n").nth(row).expect("line");

        let indent = (row + 1).to_string().len() + 1;

        // header
        write!(f, "{}", self.level.0.color(self.level.1),)?;

        if let Some(ref level_message) = self.level_message {
            writeln!(f, ": {}", level_message)?;
        } else {
            writeln!(f)?;
        }

        // source location
        writeln!(
            f,
            "{}{} {}:{}:{}",
            " ".repeat(indent - 1),
            "-->".color(Color::Blue),
            self.filename,
            row + 1,
            col + 1
        )?;

        // Above message, currently not supported cause I don't see the use for it
        writeln!(f, "{}{}", " ".repeat(indent), "|".color(Color::Blue))?;

        // code line
        writeln!(
            f,
            "{} {} {}",
            (row + 1).to_string().color(Color::Blue),
            "|".color(Color::Blue),
            line
        )?;

        // bottom context
        write!(
            f,
            "{}{}{}{}",
            " ".repeat(indent),
            "|".color(Color::Blue),
            " ".repeat(col + 1),
            "^".repeat(self.end - self.start).color(Color::Yellow)
        )?;

        if let Some(below_message) = &self.below_message {
            writeln!(f, " {}", (below_message as &str).color(Color::Yellow))?;
        } else {
            writeln!(f)?;
        }

        if let Some(note) = &self.note {
            writeln!(
                f,
                "{}{} {}: {}",
                " ".repeat(indent),
                "=".color(Color::Blue),
                "note".bold(),
                note
            )?;
        }

        Ok(())
    }
}
