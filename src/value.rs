use std::borrow::Cow;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct IntegerType {
    pub size: u32,
    pub signed: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueData<'a> {
    Integer { value: u64, ty: IntegerType },

    Identifier(Cow<'a, str>),

    String(String),

    List(Vec<Value<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Value<'a> {
    pub data: ValueData<'a>,
    pub span: (usize, usize),
}

impl<'a> ValueData<'a> {
    fn to_static(self) -> ValueData<'static> {
        match self {
            ValueData::Identifier(s) => ValueData::Identifier(Cow::Owned(s.into_owned())),
            ValueData::List(v) => ValueData::List(v.into_iter().map(Value::to_static).collect()),
            ValueData::Integer { value, ty } => ValueData::Integer { value, ty },
            ValueData::String(s) => ValueData::String(s),
        }
    }
}

impl<'a> Value<'a> {
    pub fn to_static(self) -> Value<'static> {
        Value {
            data: self.data.to_static(),
            span: self.span,
        }
    }
}
