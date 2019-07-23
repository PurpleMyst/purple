use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),

    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),

    Identifier(Cow<'a, str>),

    String(String),

    SExpr(Vec<Value<'a>>),
}

impl<'a> Value<'a> {
    pub fn to_static_lifetime(&self) -> Value<'static> {
        match self {
            Value::Identifier(s) => Value::Identifier(Cow::Owned(s.to_owned().into_owned())),
            Value::SExpr(v) => Value::SExpr(v.iter().map(Value::to_static_lifetime).collect()),

            Value::U64(n) => Value::U64(*n),
            Value::U32(n) => Value::U32(*n),
            Value::U16(n) => Value::U16(*n),
            Value::U8(n) => Value::U8(*n),

            Value::I64(n) => Value::I64(*n),
            Value::I32(n) => Value::I32(*n),
            Value::I16(n) => Value::I16(*n),
            Value::I8(n) => Value::I8(*n),

            Value::String(s) => Value::String(s.clone()),
        }
    }
}
