use std::borrow::Cow;

use colorful::{Color, Colorful};
use nom::{
    branch::alt,
    character::complete::{alpha1, anychar, char, digit1, multispace0, none_of, one_of},
    error::ErrorKind,
    sequence::delimited,
};

use crate::value::Value;

#[derive(Debug)]
pub enum ParseError<'a> {
    Or(Vec<Self>),

    FromErrorKind {
        input: &'a str,
        kind: ErrorKind,
    },

    FromChar {
        input: &'a str,
        c: char,
    },

    WithContext {
        context: &'static str,
        input: &'a str,
        error: Box<Self>,
    },

    Custom {
        context: &'static str,
        input: &'a str,
        span: usize,
    },
}

impl<'a> ParseError<'a> {
    /// Return the range of the code positions affected by this error.
    /// Indexes are returned as "negative indices", i.e. `(5, 4)` means (input.len() - 5, input.len() - 4)`
    fn span(&self) -> Option<(usize, usize)> {
        match self {
            ParseError::Or { .. } => None,

            ParseError::WithContext { input, .. }
            | ParseError::FromErrorKind { input, .. }
            | ParseError::FromChar { input, .. } => Some((input.len(), input.len() - 1)),

            ParseError::Custom { input, span, .. } => Some((input.len(), input.len() - span)),
        }
    }

    fn message(&self) -> Option<String> {
        Some(format!(
            "Expected {}",
            match self {
                ParseError::FromErrorKind { kind, .. } => kind.description().to_owned(),
                ParseError::WithContext { context, .. } => context.clone().to_owned(),
                ParseError::Custom { context, .. } => context.clone().to_owned(),
                ParseError::FromChar { c, .. } => format!("{:?}", c),
                ParseError::Or { .. } => return None,
            }
        ))
    }

    fn expected(context: &'static str, input: &'a str, span: usize) -> nom::Err<Self> {
        nom::Err::Error(ParseError::Custom {
            context,
            input,
            span,
        })
    }

    fn expected_unrecoverable(
        context: &'static str,
        input: &'a str,
        span: usize,
    ) -> nom::Err<Self> {
        nom::Err::Failure(ParseError::Custom {
            context,
            input,
            span,
        })
    }

    // TODO: group errors of a `ParseError::Or` together and display them in one "block"
    fn show(self, input: &str) -> String {
        let (start, end) = self.span().expect("span");
        let (row, col) = position(input, start);

        let line = input.split("\n").nth(row).expect("line");

        let indent = (row + 1).to_string().len() + 1;

        [
            // header
            format!(
                "{}: {}",
                "error".color(Color::Red),
                self.message().expect("expected")
            ),
            // source location
            format!(
                "{}{} <input>:{}:{}",
                " ".repeat(indent - 1),
                "-->".color(Color::Blue),
                row + 1,
                col + 1
            ),
            // above context
            format!("{}{}", " ".repeat(indent), "|".color(Color::Blue)),
            // code line
            format!(
                "{} {} {}",
                (row + 1).to_string().color(Color::Blue),
                "|".color(Color::Blue),
                line
            ),
            // bottom context
            format!(
                "{}{}{}{}",
                " ".repeat(indent),
                "|".color(Color::Blue),
                " ".repeat(col + 1),
                "^".repeat(start - end).color(Color::Yellow)
            ),
        ]
        .join("\n")
    }
}

impl<'a> nom::error::ParseError<&'a str> for ParseError<'a> {
    fn from_error_kind(input: &'a str, kind: ErrorKind) -> Self {
        ParseError::FromErrorKind { input, kind }
    }

    fn append(_input: &'a str, _kind: ErrorKind, other: Self) -> Self {
        other
    }

    fn or(self, other: Self) -> Self {
        let mut operands = if let ParseError::Or(operands) = self {
            operands
        } else {
            vec![self]
        };

        if let ParseError::Or(other_operands) = other {
            operands.extend(other_operands);
        } else {
            operands.push(other);
        }

        ParseError::Or(operands)
    }

    fn from_char(input: &'a str, c: char) -> Self {
        ParseError::FromChar { input, c }
    }

    fn add_context(input: &'a str, context: &'static str, error: Self) -> Self {
        ParseError::WithContext {
            input,
            context,
            error: Box::new(error),
        }
    }
}

type IResult<'a, T> = nom::IResult<&'a str, T, ParseError<'a>>;

macro_rules! ws {
    ($parser:expr) => {
        delimited(multispace0, $parser, multispace0);
    };
}

fn identifier(input: &str) -> IResult<Value> {
    if let Ok((input, ident)) = alpha1::<&str, ParseError<'_>>(input) {
        Ok((input, Value::Identifier(Cow::Borrowed(ident))))
    } else {
        Err(ParseError::expected("an identifier", input, 1))
    }
}

fn integer_sign(input: &str) -> IResult<bool> {
    let (new_input, signed) = one_of::<_, _, ParseError>("iu")(input)
        .map_err(|_| ParseError::expected("a signedness specifier", input, 1))?;

    Ok((
        new_input,
        match signed {
            'i' => true,
            'u' => false,
            _ => unreachable!(),
        },
    ))
}

fn integer_size(input: &str) -> IResult<u32> {
    let (new_input, size) = digit1::<_, ParseError>(input)
        .map(|(input, s)| (input, s.parse().unwrap()))
        .map_err(|_| ParseError::expected("an integer width", input, input.len() - input.len()))?;

    if size < 1 || size > 64 {
        return Err(ParseError::expected_unrecoverable(
            "an integer size between 1 and 64",
            input,
            input.len() - new_input.len(),
        ));
    }

    Ok((new_input, size))
}

// TODO: allow untyped integer literals and infer them in a pass
fn integer(input: &str) -> IResult<Value> {
    let (input, value) = digit1::<_, ParseError>(input)
        .map(|(input, s)| (input, s.parse().unwrap()))
        .map_err(|_| ParseError::expected("an integer", input, 1))?;

    let (input, signed) = integer_sign(input)?;

    let (input, size) = integer_size(input)?;

    Ok((
        input,
        Value::Integer {
            signed,
            size,
            value,
        },
    ))
}

fn string(input: &str) -> IResult<Value> {
    let (mut input, _) = char::<_, ParseError>('"')(input)
        .map_err(|_| ParseError::expected("a string", input, 1))?;
    let mut result = String::new();

    loop {
        let (new_input, c) = anychar(input)?;
        input = new_input;

        if c == '"' {
            return Ok((input, Value::String(result)));
        }

        result.push(c);
    }
}

fn value(input: &str) -> IResult<Value> {
    alt((string, integer, identifier, sexpr))(input)
}

// FIXME: Show proper error here, not just `Expected ')'`.
fn sexpr(input: &str) -> IResult<Value> {
    let (mut input, _) = ws!(char('('))(input)?;

    let mut contents = Vec::new();
    loop {
        match ws!(value)(input) {
            Ok((new_input, item)) => {
                input = new_input;
                contents.push(item);
            }

            Err(error) => match ws!(char::<_, ParseError<'_>>(')'))(input) {
                Ok((input, _)) => return Ok((input, Value::SExpr(contents))),
                Err(_) => return Err(error),
            },
        }
    }
}

fn position(text: &str, remaining: usize) -> (usize, usize) {
    text.chars()
        .take(text.len() - remaining)
        .fold((0, 0), |(row, col), c| {
            if c == '\n' {
                (row + 1, 0)
            } else {
                (row, col + 1)
            }
        })
}

pub fn parse(input: &str) -> Result<Value, String> {
    let error = match sexpr(input) {
        Ok(("", value)) => return Ok(value),
        Ok((rest, _)) => ParseError::Custom {
            input: rest,
            span: 1,
            context: "EOF",
        },
        Err(nom::Err::Error(error)) | Err(nom::Err::Failure(error)) => error,
        Err(nom::Err::Incomplete(..)) => unreachable!("only nom::*::complete functions are used"),
    };

    if let ParseError::Or(operands) = error {
        Err(operands
            .into_iter()
            .map(|error| error.show(input))
            .collect::<Vec<_>>()
            .join("\n\n"))
    } else {
        Err(error.show(input))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_identifier(s in "[a-zA-Z]+") {
            let ("", Value::Identifier(s2)) = identifier(&s).unwrap();
            prop_assert_eq!(&s2, &s);
        }

        #[test]
        #[ignore] // FIXME: take into consideration  integer width!
        fn test_number(ns in "([0-9]|[1-9][0-9]{0,0})", ss in "[iu](8|16|32|64)") {
            let ws = ns.clone() + &ss;
            let (rest, v) = integer(&ws).unwrap();

            prop_assert_eq!(rest, "");

            prop_assert_eq!(ns, if let Value::Integer { value, .. } = v {
                // FIXME: Properly display negative integers
                value.to_string()
            } else {
                unreachable!()
            });
        }

        #[test]
        fn test_string(s in "[^\"]*") {
            let wrapped_s = "\"".to_owned() + &s + "\"";
            if let ("", Value::String(s2)) = string(&wrapped_s).unwrap() {
                prop_assert_eq!(s2, s);
            } else {
                unreachable!()
            }
        }
    }
}
