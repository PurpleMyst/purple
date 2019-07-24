use std::collections::{HashMap, VecDeque};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::value::Value;

#[derive(Default, Debug)]
struct Frame<'a> {
    variables: HashMap<&'a str, PointerValue>,
}

#[derive(Debug)]
pub struct Compiler<'a> {
    context: Context,
    module: Module,
    builder: Builder,

    frames: VecDeque<Frame<'a>>,
}

// FIXME: Include span information?
#[derive(Debug)]
pub enum CompileError {
    UndefinedVariable(String),
    InvalidType(Value<'static>),
    InvalidShape(Value<'static>, &'static str),
}

pub type Result<T, E = CompileError> = std::result::Result<T, E>;

macro_rules! expect_variant {
    ($var:expr => $variant:ident) => {{
        let var = $var;
        if let Value::$variant(x) = var {
            Ok(x)
        } else {
            let err: CompileError = CompileError::InvalidType(var.to_static_lifetime());
            Err(err)
        }
    }};
}

macro_rules! with_basic_block {
    ($parent:ident.$builder:ident: $temp_basic_block:expr => $body:block) => {{
        let old_block = $parent.$builder.get_insert_block();
        $parent.$builder.position_at_end(&$temp_basic_block);
        let result = $body;
        if let Some(ref old_block) = old_block {
            $parent.$builder.position_at_end(old_block);
        }
        result
    }};
}

impl<'a> Compiler<'a> {
    pub fn new(module_name: &str) -> Self {
        let context = Context::create();
        let mut frames = VecDeque::new();
        frames.push_back(Frame::default());

        Self {
            module: context.create_module(module_name),
            builder: context.create_builder(),
            context,
            frames,
        }
    }

    fn compile_identifier(&mut self, ident: &'_ str) -> Result<BasicValueEnum> {
        self.frames
            .iter()
            .find_map(|frame| frame.variables.get(ident))
            .map(|value| value.as_basic_value_enum())
            .ok_or_else(|| CompileError::UndefinedVariable(ident.to_owned()))
    }

    fn get_type_from_value(&self, name: &Value<'_>) -> Result<BasicTypeEnum> {
        let name: &str = expect_variant!(name => Identifier)?;

        Ok(match name {
            "int" => self.context.i32_type().as_basic_type_enum(),
            _ => unimplemented!(),
        })
    }

    fn compile_function(
        &mut self,
        name: &str,
        parameters: &[Value<'a>],
        return_type: BasicTypeEnum,
        body: &[Value<'a>],
    ) -> Result<FunctionValue> {
        let parameter_types = parameters
            .iter()
            .map(|sexpr| {
                self.get_type_from_value(expect_variant!(sexpr => SExpr)?.get(0).ok_or_else(
                    || {
                        CompileError::InvalidShape(
                            sexpr.to_static_lifetime(),
                            "two-item s expression",
                        )
                    },
                )?)
            })
            .collect::<Result<Vec<BasicTypeEnum>>>()?;

        let function_type = return_type.fn_type(&parameter_types, /*is_var_args:*/ false);
        let function = self
            .module
            .add_function(name, function_type, /*linkage:*/ None);

        let entry_block = self.context.append_basic_block(&function, "entry");

        // FIXME: create a new frame + assign the arguments

        with_basic_block!(self.builder: entry_block => {
            let body_values = body.into_iter().cloned().map(|v| self.compile(v)).collect::<Result<Vec<BasicValueEnum>>>()?;
            let return_value = body_values.last();

            // FIXME: check the type of the return value

            self.builder.build_return(return_value.map(|v| v as _));
        });

        Ok(function)
    }

    fn compile_sexpr(&mut self, sexpr: Vec<Value<'a>>) -> Result<BasicValueEnum> {
        let first_ident: &str = expect_variant!(sexpr.get(0).unwrap() => Identifier)?.as_ref();

        Ok(match first_ident {
            "function" => {
                let name = sexpr.get(1).unwrap();
                let parameters = sexpr.get(2).unwrap();
                let return_type = sexpr.get(3).unwrap();
                let body = &sexpr[4..];

                self.compile_function(
                    expect_variant!(name => Identifier)?,
                    expect_variant!(parameters => SExpr)?,
                    self.get_type_from_value(return_type)?,
                    body,
                )?
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum()
            }

            _ => unimplemented!(),
        })
    }

    pub fn compile(&mut self, value: Value<'a>) -> Result<BasicValueEnum> {
        Ok(match value {
            Value::Integer {
                value,
                signed,
                size,
            } => self
                .context
                .custom_width_int_type(size)
                .const_int(value, signed)
                .as_basic_value_enum(),

            Value::String(s) => self
                .context
                .const_string(&s, /*null_terminated:*/ false)
                .as_basic_value_enum(),
            Value::Identifier(s) => self.compile_identifier(s.as_ref())?,
            Value::SExpr(v) => self.compile_sexpr(v)?,
        })
    }
}

#[cfg(test)]
mod tests {}
