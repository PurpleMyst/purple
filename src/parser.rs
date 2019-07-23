use std::borrow::Cow;

use colorful::{Color, Colorful};
use nom::{
    branch::alt,
    character::complete::{alpha1, anychar, char, digit1, multispace0, none_of, one_of},
    combinator::{map, map_res},
    error::{context, ErrorKind},
    multi::many0,
    sequence::delimited,
};

use crate::value::Value;

#[derive(Debug)]
pub enum ParseError<'a> {
    Or {
        operands: Vec<Self>,
    },

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

    fn expected(&self) -> Option<String> {
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
}

impl<'a> nom::error::ParseError<&'a str> for ParseError<'a> {
    fn from_error_kind(input: &'a str, kind: ErrorKind) -> Self {
        ParseError::FromErrorKind { input, kind }
    }

    fn append(_input: &'a str, _kind: ErrorKind, other: Self) -> Self {
        other
    }

    fn or(self, other: Self) -> Self {
        let mut operands = if let ParseError::Or { operands } = self {
            operands
        } else {
            vec![self]
        };

        if let ParseError::Or {
            operands: other_operands,
        } = other
        {
            operands.extend(other_operands);
        } else {
            operands.push(other);
        }

        ParseError::Or { operands }
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
        Err(nom::Err::Error(ParseError::Custom {
            context: "an identifier",
            span: 1,
            input,
        }))
    }
}

// TODO: allow untyped integer literals and infer them in a pass
fn number(input: &str) -> IResult<Value> {
    let start_input = input;

    let (input, n) = digit1::<_, ParseError>(input)
        .map(|(input, s)| (input, s.parse().unwrap()))
        .map_err(|_| {
            nom::Err::Error(ParseError::Custom {
                context: "an integer",
                span: 1,
                input,
            })
        })?;

    let (input, signedness) = one_of::<_, _, ParseError>("iu")(input).map_err(|_| {
        nom::Err::Error(ParseError::Custom {
            context: "a signedness specifier",
            span: start_input.len() - input.len(),
            input: start_input,
        })
    })?;

    let (input, width) = digit1::<_, ParseError>(input)
        .map(|(input, s)| (input, s.parse().unwrap()))
        .map_err(|_| {
            nom::Err::Error(ParseError::Custom {
                context: "an integer width",
                span: start_input.len() - input.len(),
                input: start_input,
            })
        })?;

    let value = match (signedness, width) {
        ('u', 64) => Value::U64(n),
        ('u', 32) => Value::U32(n as u32),
        ('u', 16) => Value::U16(n as u16),
        ('u', 8) => Value::U8(n as u8),

        ('i', 64) => Value::I64(n as i64),
        ('i', 32) => Value::I32(n as i32),
        ('i', 16) => Value::I16(n as i16),
        ('i', 8) => Value::I8(n as i8),

        _ => {
            return Err(nom::Err::Failure(ParseError::Custom {
                context: "a valid integer width",
                span: start_input.len() - input.len(),
                input: start_input,
            }))
        }
    };

    Ok((input, value))
}

fn string(input: &str) -> IResult<Value> {
    let (mut input, _) = char::<_, ParseError>('"')(input).map_err(|_| {
        nom::Err::Error(ParseError::Custom {
            context: "a string",
            span: 1,
            input,
        })
    })?;
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
    alt((string, number, identifier, sexpr))(input)
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

// TODO: group errors of a `ParseError::Or` together and display them in one "block"
fn rusty_error(input: &str, error: ParseError<'_>) -> String {
    let (start, end) = error.span().expect("span");
    let (row, col) = position(input, start);

    let line = input.split("\n").nth(row).expect("line");

    let indent = (row + 1).to_string().len() + 1;

    [
        // header
        format!(
            "{}: {}",
            "error".color(Color::Red),
            error.expected().expect("expected")
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

pub fn parse(input: &str) -> Result<Value, String> {
    // error: cannot find macro `xunimplemented!` in this scope
    //    --> src/parser.rs:113:23
    //     |
    // 113 |         Ok((_, _)) => xunimplemented!(),
    //     |                       ^^^^^^^^^^^^^^ help: you could try the macro: `unimplemented`

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

    if let ParseError::Or { operands } = error {
        Err(operands
            .into_iter()
            .map(|error| rusty_error(input, error))
            .collect::<Vec<_>>()
            .join("\n\n"))
    } else {
        Err(rusty_error(input, error))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_identifier(s in "[a-zA-Z]+") {
            if let ("", Value::Identifier(s2)) = identifier(&s).unwrap() {
                prop_assert_eq!(&s2, &s);
            } else {
                unreachable!()
            }
        }

        #[test]
        #[ignore] // FIXME: take into consideration  integer width!
        fn test_number(ns in "([0-9]|[1-9][0-9]{0,0})", ss in "[iu](8|16|32|64)") {
            let ws = ns.clone() + &ss;
            let (rest, v) = number(&ws).unwrap();

            prop_assert_eq!(rest, "");

            prop_assert_eq!(ns, match v {
                Value::U64(n) => n.to_string(),
                Value::U32(n) => n.to_string(),
                Value::U16(n) => n.to_string(),
                Value::U8(n) => n.to_string(),

                Value::I64(n) => n.to_string(),
                Value::I32(n) => n.to_string(),
                Value::I16(n) => n.to_string(),
                Value::I8(n) => n.to_string(),

                _ => unreachable!(),
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
