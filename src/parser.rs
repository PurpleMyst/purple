use std::borrow::Cow;

use nom::{
    branch::alt,
    character::complete::{alpha1, char, digit1, multispace0, none_of, one_of},
    combinator::{map, map_opt, map_res},
    error::VerboseError,
    multi::many0,
    sequence::{delimited, tuple},
};

use crate::value::Value;

type IResult<'a, T> = nom::IResult<&'a str, T, VerboseError<&'a str>>;

macro_rules! ws {
    ($parser:expr) => {
        delimited(multispace0, $parser, multispace0);
    };
}

fn identifier(input: &str) -> IResult<Value> {
    map(alpha1, |s| Value::Identifier(Cow::Borrowed(s)))(input)
}

// TODO: allow untyped integer literals and infer them in a pass
fn number(input: &str) -> IResult<Value> {
    map_opt(
        tuple((
            map_res(digit1, str::parse),
            one_of("iu"),
            map_res(digit1, str::parse),
        )),
        |n: (u64, char, u8)| match n {
            (n, 'u', 64) => Some(Value::U64(n as u64)),
            (n, 'u', 32) => Some(Value::U32(n as u32)),
            (n, 'u', 16) => Some(Value::U16(n as u16)),
            (n, 'u', 8) => Some(Value::U8(n as u8)),

            (n, 'i', 64) => Some(Value::I64(n as i64)),
            (n, 'i', 32) => Some(Value::I32(n as i32)),
            (n, 'i', 16) => Some(Value::I16(n as i16)),
            (n, 'i', 8) => Some(Value::I8(n as i8)),

            _ => None,
        },
    )(input)
}

fn string(input: &str) -> IResult<Value> {
    map(delimited(char('"'), many0(none_of("\"")), char('"')), |s| {
        Value::String(s.into_iter().collect())
    })(input)
}

fn value(input: &str) -> IResult<Value> {
    alt((string, number, identifier, sexpr))(input)
}

fn sexpr(input: &str) -> IResult<Value> {
    map(
        delimited(ws!(char('(')), many0(ws!(value)), ws!(char(')'))),
        Value::SExpr,
    )(input)
}

pub fn parse(input: &str) -> IResult<Value> {
    value(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_identifier(s in "[a-zA-Z]+") {
            if let Ok(("", Value::Identifier(s2))) = identifier(&s) {
                prop_assert_eq!(s2, &s);
            } else {
                unreachable!()
            }
        }

        #[test]
        fn test_number(ns in "[0-9]|[1-9][0-9]{0,10}") {
            if let Ok(("", Value::U64(n))) = number(&ns) {
                prop_assert_eq!(n.to_string(), ns);
            } else {
                unreachable!()
            }
        }

        #[test]
        fn test_string(s in "[^\"]*") {
            let wrapped_s = "\"".to_owned() + &s + "\"";
            if let Ok(("", Value::String(s2))) = string(&wrapped_s) {
                prop_assert_eq!(s2, s);
            } else {
                unreachable!()
            }
        }
    }
}
