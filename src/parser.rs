use nom::{
    branch::alt,
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::{map, map_res},
    error::VerboseError,
    multi::many0,
    sequence::{delimited, preceded},
};

#[derive(PartialEq, Debug)]
pub enum Value<'a> {
    Number(f64),
    Identifier(&'a str),
    SExpr(Vec<Value<'a>>),
}

type IResult<'a, T> = nom::IResult<&'a str, T, VerboseError<&'a str>>;

macro_rules! ws {
    ($parser:expr) => {
        delimited(multispace0, $parser, multispace0);
    };
}

fn identifier(input: &str) -> IResult<Value> {
    map(alpha1, Value::Identifier)(input)
}

fn number(input: &str) -> IResult<Value> {
    map(map_res(digit1, str::parse), Value::Number)(input)
}

fn quoted_value(input: &str) -> IResult<Value> {
    map(preceded(char('\''), value), |v| {
        Value::SExpr(vec![Value::Identifier("quote"), v])
    })(input)
}

fn value(input: &str) -> IResult<Value> {
    alt((quoted_value, number, identifier, sexpr))(input)
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

    macro_rules! proptest_combinator {
        (|$var:ident in $strategy:literal| $block:block == $expected_result:expr) => {
            proptest!(|($var in $strategy)| {
                let (rest, result) = $block.unwrap();
                prop_assert_eq!(rest, "");
                prop_assert_eq!(result, $expected_result)
            });
        }
    }

    #[test]
    fn t_identifier() {
        proptest_combinator!(|s in "[a-zA-Z]+"| { identifier(&s) } == Value::Identifier(&s))
    }

    #[test]
    fn t_number() {
        proptest_combinator!(|s in "[0-9]|[1-9][0-9]{0,15}"| {
            map(number, |v| if let Value::Number(n) = v { n.to_string() } else { unreachable!() } )(&s)
        } == s.clone())
    }
}
