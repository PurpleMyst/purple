use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
pub enum ValueData<'a> {
    Integer(u64),

    Identifier(Cow<'a, str>),

    String(String),

    List(Vec<Value<'a>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueType {
    Integer { size: u32, signed: bool },
    Identifier,
    String,
    List,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Value<'a> {
    pub data: ValueData<'a>,
    pub ty: Option<ValueType>,
    pub span: (usize, usize),
}

impl<'a> ValueData<'a> {
    fn to_static(self) -> ValueData<'static> {
        match self {
            ValueData::Identifier(s) => ValueData::Identifier(Cow::Owned(s.into_owned())),
            ValueData::List(v) => ValueData::List(v.into_iter().map(Value::to_static).collect()),
            ValueData::Integer(value) => ValueData::Integer(value),
            ValueData::String(s) => ValueData::String(s),
        }
    }
}

impl<'a> Value<'a> {
    pub fn to_static(self) -> Value<'static> {
        Value {
            data: self.data.to_static(),
            ty: self.ty,
            span: self.span,
        }
    }
}
