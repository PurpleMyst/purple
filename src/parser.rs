use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::take_while,
    character::complete::{anychar, char, digit1, multispace0, one_of},
    error::ErrorKind,
    sequence::delimited,
};

use crate::{
    diagnostic::Diagnostic,
    value::{IntegerType, Value},
};

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
    const IDENTIFIER_START_CHARACTERS: &'static str = concat!(
        "abcdefghijklmnopqrstuvwxyz",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "_-",
    );

    const IDENTIFIER_CHARACTERS: &'static str = concat!(
        "abcdefghijklmnopqrstuvwxyz",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "_-",
        "0123456789",
    );

    // We're just checking if the character is an identifier start character, we utilize the next
    // line to actually get the identifier characters
    one_of::<_, _, ParseError>(IDENTIFIER_START_CHARACTERS)(input)
        .map_err(|_| ParseError::expected("an identifier", input, 1))?;

    let (input, ident) = take_while(|c| IDENTIFIER_CHARACTERS.contains(c))(input)?;

    Ok((input, Value::Identifier(Cow::Borrowed(ident))))
}

fn integer_sign(input: &str) -> IResult<bool> {
    let (new_input, signed) = one_of::<_, _, ParseError>("iu")(input)
        .map_err(|_| ParseError::expected_unrecoverable("either 'i' or 'u'", input, 1))?;

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

// TODO: allow unspecified-size integer literals and infer them in a pass
// TODO: put out a warning(error?) if something like `2i32x` is typed
fn integer(input: &str) -> IResult<Value> {
    let (input, value) = digit1::<_, ParseError>(input)
        .map(|(input, s)| (input, s.parse().unwrap()))
        .map_err(|_| ParseError::expected("an integer", input, 1))?;

    let (input, signed) = integer_sign(input)?;

    let (input, size) = integer_size(input)?;

    Ok((
        input,
        Value::Integer {
            value,
            ty: IntegerType { signed, size },
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

pub fn parse(filename: &str) -> Result<Value, Vec<Diagnostic>> {
    // FIXME: better error if filename does not exist
    let input = std::fs::read_to_string(filename).unwrap();

    let error = match sexpr(&input) {
        Ok(("", value)) => return Ok(value.to_static_lifetime()),
        Ok((rest, _)) => ParseError::Custom {
            input: rest,
            span: 1,
            context: "EOF",
        },
        Err(nom::Err::Error(error)) | Err(nom::Err::Failure(error)) => error,
        Err(nom::Err::Incomplete(..)) => unreachable!("only nom::*::complete functions are used"),
    };

    let to_diagnostic = |error: ParseError| {
        let (start, end) = error.span().unwrap();

        Diagnostic {
            input: input.to_owned(),
            filename: filename.to_owned(),
            start: input.len() - start,
            end: input.len() - end,
            level: ("error", colorful::Color::Red),
            level_message: Some(error.message().unwrap()),
            below_message: None,
            note: None,
        }
    };

    if let ParseError::Or(operands) = error {
        Err(operands.into_iter().map(to_diagnostic).collect::<Vec<_>>())
    } else {
        Err(vec![to_diagnostic(error)])
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
