#[derive(Clone, Debug, PartialEq)]
pub enum ValueData<'a> {
    Integer(u64),

    Identifier(&'a str),

    String(String),

    List(Vec<Value<'a>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueType {
    Integer {
        size: Option<u32>,
        signed: Option<bool>,
    },
    Identifier,
    String,
    List,

    Function {
        parameter_types: Vec<ValueType>,
        return_type: Box<ValueType>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Value<'a> {
    pub data: ValueData<'a>,
    pub ty: ValueType,
    pub span: (usize, usize),
}
