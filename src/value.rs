use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    Integer { value: u64, size: u8, signed: bool },

    Identifier(Cow<'a, str>),

    String(String),

    SExpr(Vec<Value<'a>>),
}

impl<'a> Value<'a> {
    pub fn to_static_lifetime(&self) -> Value<'static> {
        match self {
            Value::Identifier(s) => Value::Identifier(Cow::Owned(s.to_owned().into_owned())),
            Value::SExpr(v) => Value::SExpr(v.iter().map(Value::to_static_lifetime).collect()),
            Value::Integer {
                value,
                size,
                signed,
            } => Value::Integer {
                value: *value,
                size: *size,
                signed: *signed,
            },
            Value::String(s) => Value::String(s.clone()),
        }
    }
}
