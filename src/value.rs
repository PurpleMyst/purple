#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    U64(u64),
    Identifier(&'a str),
    String(String),
    SExpr(Vec<Value<'a>>),
}
