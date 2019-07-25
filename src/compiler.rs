use std::collections::{HashMap, VecDeque};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    diagnostic::Diagnostic,
    value::{Value, ValueData, ValueType},
};

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

#[derive(Debug)]
pub struct CompileError {
    message: &'static str,
    span: (usize, usize),
}

impl From<CompileError> for Diagnostic {
    fn from(error: CompileError) -> Diagnostic {
        Diagnostic {
            span: error.span,
            level: ("error", colorful::Color::Red),
            level_message: Some(error.message.to_owned()),
            below_message: None,
            note: None,
        }
    }
}

pub type Result<T, E = CompileError> = std::result::Result<T, E>;

// XXX: Can we unify `variant!` and `variant_ref!`?
macro_rules! variant {
    ($var:expr => $variant:ident) => {{
        let var = $var;
        let span = var.span;
        if let ValueData::$variant(x) = var.data {
            Ok(x)
        } else {
            Err(CompileError {
                message: concat!("Expected value of type ", stringify!($variant)),
                span: span,
            })
        }
    }};
}

macro_rules! variant_ref {
    ($var:expr => $variant:ident) => {{
        let var = $var;
        let span = var.span;
        if let ValueData::$variant(ref x) = var.data {
            Ok(x)
        } else {
            Err(CompileError {
                message: concat!("Expected value of type ", stringify!($variant)),
                span: span,
            })
        }
    }};
}

/// Temporarly move a builder into a basic block and restore it at the end of the block
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

    fn compile_identifier(&mut self, ident: &str, span: (usize, usize)) -> Result<BasicValueEnum> {
        self.frames
            .iter()
            .find_map(|frame| frame.variables.get(ident))
            .map(|value| value.as_basic_value_enum())
            .ok_or_else(|| CompileError {
                message: "Undefined variable",
                span,
            })
    }

    fn parse_typename(&self, name: &Value<'_>) -> Result<ValueType> {
        Ok(match variant_ref!(name => Identifier)? as &str {
            // FIXME: handle the same integer types as the parser
            "i32" => ValueType::Integer {
                size: 32,
                signed: true,
            },

            _ => {
                return Err(CompileError {
                    message: "Unknown type",
                    span: name.span,
                })
            }
        })
    }

    // FIXME: Make `CompileError` take a Cow<str> and make non-shit error messages here.
    fn check_type(&self, ty: ValueType, value: &mut Value) -> Result<()> {
        // If the two types are equal, then we're fine
        if Some(ty) == value.ty {
            return Ok(());
        }

        // If the value has no type, we must infer it
        if value.ty.is_none() {
            if let ValueData::Integer(_) = value.data {
                value.ty = Some(ty);
                Ok(())
            } else {
                Err(CompileError {
                    message: "Could not infer type",
                    span: value.span,
                })
            }
        } else {
            Err(CompileError {
                message: "Wrong type",
                span: value.span,
            })
        }
    }

    fn to_llvm_type(&self, ty: ValueType) -> BasicTypeEnum {
        match ty {
            ValueType::Integer { size, .. } => self
                .context
                .custom_width_int_type(size)
                .as_basic_type_enum(),

            _ => unimplemented!("to_llvm_type({:?})", ty),
        }
    }

    fn compile_function(&mut self, mut value: Vec<Value<'a>>) -> Result<FunctionValue> {
        assert_eq!(variant!(value.remove(0) => Identifier)?, "function");

        let name = variant!(value.remove(0) => Identifier)?;
        let parameters = value.remove(0);
        let return_type = self.parse_typename(&value.remove(0))?;
        let mut body = value;

        let parameter_types = variant_ref!(&parameters => List)?
            .iter()
            .map(|sexpr| {
                self.parse_typename(variant_ref!(sexpr => List)?.get(0).ok_or_else(|| {
                    CompileError {
                        message:
                            "Expected a two-length list containing the type and the argument name",
                        span: sexpr.span,
                    }
                })?)
                .map(|ty| self.to_llvm_type(ty))
            })
            .collect::<Result<Vec<BasicTypeEnum>>>()?;

        let function_type = self
            .to_llvm_type(return_type)
            .fn_type(&parameter_types, /*is_var_args:*/ false);

        let function = self
            .module
            .add_function(&name, function_type, /*linkage:*/ None);

        let entry_block = self.context.append_basic_block(&function, "entry");

        // FIXME: create a new frame + assign the arguments
        if !variant_ref!(&parameters => List)?.is_empty() {
            return Err(CompileError {
                message: "Parameters are not supported yet",
                span: parameters.span,
            });
        }

        // FIXME: proper error
        self.check_type(return_type.clone(), body.last_mut().unwrap())?;

        with_basic_block!(self.builder: entry_block => {
            let body_values = body.into_iter().map(|v| self.compile(v.clone())).collect::<Result<Vec<BasicValueEnum>>>()?;
            let return_value = body_values.last();

            self.builder.build_return(return_value.map(|v| v as _));
        });

        Ok(function)
    }

    fn compile_sexpr(&mut self, sexpr: Vec<Value<'a>>) -> Result<BasicValueEnum> {
        let first_ident: &str = variant_ref!(sexpr.get(0).unwrap() => Identifier)?.as_ref();

        Ok(match first_ident {
            "function" => self
                .compile_function(sexpr)?
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum(),

            _ => unimplemented!(),
        })
    }

    pub fn compile(&mut self, value: Value<'a>) -> Result<BasicValueEnum> {
        Ok(match value {
            Value {
                data: ValueData::Integer(value),
                ty: Some(ValueType::Integer { size, signed }),
                ..
            } => self
                .context
                .custom_width_int_type(size)
                .const_int(value, signed)
                .as_basic_value_enum(),

            Value {
                data: ValueData::Integer(_),
                ty: Some(_),
                span,
            } => {
                return Err(CompileError {
                    // TODO: make a better error message, even though it's probably unreachable
                    message: "The type of this integer value makes no sense",
                    span,
                });
            }

            Value {
                data: ValueData::Integer(_),
                ty: None,
                span,
            } => {
                return Err(CompileError {
                    message: "Could not infer type for integer value",
                    span,
                })
            }

            Value {
                data: ValueData::String(s),
                ..
            } => self
                .context
                .const_string(&s, /*null_terminated:*/ false)
                .as_basic_value_enum(),

            Value {
                data: ValueData::Identifier(s),
                ..
            } => self.compile_identifier(s.as_ref(), value.span)?,

            Value {
                data: ValueData::List(v),
                ..
            } => self.compile_sexpr(v)?,
        })
    }
}

#[cfg(test)]
mod tests {}
