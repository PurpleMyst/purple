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
    value::{IntegerType, Value, ValueData},
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

// NOTE: This returns a "negative index" span, to be converted in `self::parse`.
fn calculate_span(start_input: &str, end_input: &str) -> (usize, usize) {
    (start_input.len(), end_input.len())
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

    let start_input = input;

    // We're just checking if the character is an identifier start character, we utilize the next
    // line to actually get the identifier characters
    one_of::<_, _, ParseError>(IDENTIFIER_START_CHARACTERS)(input)
        .map_err(|_| ParseError::expected("an identifier", input, 1))?;

    let (input, ident) = take_while(|c| IDENTIFIER_CHARACTERS.contains(c))(input)?;

    let data = ValueData::Identifier(Cow::Borrowed(ident));
    let span = calculate_span(start_input, input);

    Ok((input, Value { data, span }))
}

fn integer_sign(input: &str) -> IResult<bool> {
    let (new_input, signed) = one_of::<_, _, ParseError>("iu")(input)
        .map_err(|_| ParseError::expected_unrecoverable("either 'i' or 'u'", input, 1))?;

    Ok((
        new_input,
        match signed {
            'i' => true,
            'u' => false,
            _ => unreachable!("the one_of above only matches i and u"),
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
    let start_input = input;

    let (input, value) = digit1::<_, ParseError>(input)
        .map(|(input, s)| (input, s.parse().unwrap()))
        .map_err(|_| ParseError::expected("an integer", input, 1))?;

    let (input, signed) = integer_sign(input)?;

    let (input, size) = integer_size(input)?;

    let data = ValueData::Integer {
        value,
        ty: IntegerType { signed, size },
    };
    let span = calculate_span(start_input, input);

    Ok((input, Value { data, span }))
}

fn string(input: &str) -> IResult<Value> {
    let start_input = input;

    let (mut input, _) = char::<_, ParseError>('"')(input)
        .map_err(|_| ParseError::expected("a string", input, 1))?;

    let mut result = String::new();
    loop {
        let (new_input, c) = anychar(input)?;
        input = new_input;

        if c == '"' {
            let data = ValueData::String(result);
            let span = calculate_span(start_input, input);

            return Ok((input, Value { data, span }));
        }

        result.push(c);
    }
}

fn value(input: &str) -> IResult<Value> {
    alt((string, integer, identifier, list))(input)
}

fn list(input: &str) -> IResult<Value> {
    let start_input = input;

    let (mut input, _) = ws!(char('('))(input)?;

    let mut contents = Vec::new();
    loop {
        match ws!(value)(input) {
            Ok((new_input, item)) => {
                input = new_input;
                contents.push(item);
            }

            // We check if there is a closing parenthesis afterwards so we can report a more
            // correct error in the case there isn't. If we got an error but we have a ')' it just
            // means the list is over.
            Err(error) => match ws!(char::<_, ParseError<'_>>(')'))(input) {
                Ok((input, _)) => {
                    let data = ValueData::List(contents);
                    let span = calculate_span(start_input, input);
                    return Ok((input, Value { data, span }));
                }

                Err(_) => return Err(error),
            },
        }
    }
}

pub fn parse(input: &str) -> Result<Value, Vec<Diagnostic>> {
    fn fix_span(input: &str, value: &mut Value) {
        value.span = (input.len() - value.span.0, input.len() - value.span.1);

        if let ValueData::List(ref mut v) = value.data {
            v.iter_mut().for_each(|v| fix_span(input, v));
        }
    }

    let error = match list(&input) {
        Ok(("", mut value)) => {
            fix_span(&input, &mut value);
            return Ok(value.to_static());
        }

        Ok((rest, _)) => ParseError::Custom {
            context: "EOF",
            input: rest,
            span: 1,
        },

        Err(nom::Err::Error(error)) | Err(nom::Err::Failure(error)) => error,

        Err(nom::Err::Incomplete(..)) => unreachable!("only nom::*::complete functions are used"),
    };

    let to_diagnostic = |error: ParseError| {
        let (start, end) = error.span().unwrap();

        Diagnostic {
            span: (input.len() - start, input.len() - end),
            level: ("error", colorful::Color::Red),
            level_message: Some(error.message().unwrap()),
            below_message: None,
            note: None,
        }
    };

    if let ParseError::Or(operands) = error {
        Err(operands.into_iter().map(to_diagnostic).collect())
    } else {
        Err(vec![to_diagnostic(error)])
    }
}
