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
    typechecker::parse_typename,
    value::{Value, ValueData, ValueType},
};

#[derive(Default, Debug)]
struct Frame<'a> {
    variables: HashMap<&'a str, BasicValueEnum>,
}

#[derive(Debug)]
struct Compiler<'a> {
    context: Context,
    pub(crate) module: Module,
    builder: Builder,

    frames: VecDeque<Frame<'a>>,
}

type Result<T, E = Diagnostic> = std::result::Result<T, E>;

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
    fn new(module_name: &str) -> Self {
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

    fn compile_identifier(&self, value: Value<'a>) -> Result<BasicValueEnum> {
        let ident = variant!(&value => Identifier)?;

        self.frames
            .iter()
            .find_map(|frame| frame.variables.get(ident))
            .copied()
            .ok_or_else(|| {
                Diagnostic::new(("error", colorful::Color::Red), value.span)
                    .level_message(format!("Undefined variable {:?}", ident))
            })
    }

    fn to_llvm_type(&self, ty: ValueType) -> BasicTypeEnum {
        match ty {
            ValueType::Integer {
                size: Some(size), ..
            } => self
                .context
                .custom_width_int_type(size)
                .as_basic_type_enum(),

            _ => unimplemented!("to_llvm_type({:?})", ty),
        }
    }

    fn compile_function_definition(&mut self, value: Vec<Value<'a>>) -> Result<FunctionValue> {
        let mut value = value.into_iter();

        assert_eq!(variant!(value.next().unwrap() => Identifier)?, "function");

        let name = variant!(value.next().unwrap() => Identifier)?;
        let parameters = variant!(value.next().unwrap() => List)?;
        let return_type = parse_typename(&value.next().unwrap())?;
        let body = value;

        let parameter_types = parameters
            .iter()
            .map(|parameter| {
                Ok(self.to_llvm_type(parse_typename(
                    variant_ref!(parameter => List)?.get(0).ok_or_else(|| {
                        Diagnostic::new(("error", colorful::Color::Red), parameter.span)
                            .level_message("Expected a parameter type")
                    })?,
                )?))
            })
            .collect::<Result<Vec<BasicTypeEnum>>>()?;

        let parameter_names = parameters
            .iter()
            .map(|parameter| {
                variant!(
                variant_ref!(parameter => List)?
                    .get(1)
                    .ok_or_else(|| Diagnostic::new(("error", colorful::Color::Red), parameter.span)
                    .level_message("Expected a parameter name"))?
                => Identifier)
            })
            .collect::<Result<Vec<&str>>>()?;

        let function_type = self
            .to_llvm_type(return_type)
            .fn_type(&parameter_types, /*is_var_args:*/ false);

        let function = self
            .module
            .add_function(&name, function_type, /*linkage:*/ None);

        // We insert it before compilation to allow for recursion
        self.frames.back_mut().unwrap().variables.insert(
            name,
            function
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum(),
        );

        let entry_block = self.context.append_basic_block(&function, "entry");

        self.frames.push_front(Frame {
            variables: parameter_names
                .into_iter()
                .zip(function.get_params().into_iter())
                .collect(),
        });

        with_basic_block!(self.builder: entry_block => {
            let body_values = body.into_iter().map(|v| self.compile(v.clone())).collect::<Result<Vec<BasicValueEnum>>>()?;
            let return_value = body_values.last();

            self.builder.build_return(return_value.map(|v| v as _));
        });

        self.frames.pop_front();

        Ok(function)
    }

    fn compile_function_call(
        &mut self,
        list: Vec<Value<'a>>,
        span: (usize, usize),
    ) -> Result<BasicValueEnum> {
        let mut list = list.into_iter();

        let head = list.next().unwrap();
        let head_span = head.span;

        let function = match self.compile(head)? {
            BasicValueEnum::PointerValue(ptr) => ptr,
            _ => {
                return Err(Diagnostic::new(("error", colorful::Color::Red), head_span)
                    .level_message("Expected a function pointer"))
            }
        };

        let parameters = list
            .map(|value| self.compile(value))
            .collect::<Result<Vec<BasicValueEnum>>>()?;

        self.builder
            .build_call(function, parameters.as_slice(), "")
            .try_as_basic_value()
            .left()
            .ok_or_else(|| {
                Diagnostic::new(("error", colorful::Color::Red), span)
                    .level_message("Expected a non-void return type")
            })
    }

    fn compile_list(&mut self, value: Value<'a>) -> Result<BasicValueEnum> {
        let span = value.span;
        let list = variant!(value => List)?;

        Ok(match &list[0].data {
            ValueData::Identifier("function") => self
                .compile_function_definition(list)?
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum(),

            ValueData::Identifier("begin") => list
                .into_iter()
                .skip(1)
                .map(|value| self.compile(value))
                .collect::<Result<Vec<_>>>()?
                .pop()
                .ok_or_else(|| {
                    Diagnostic::new(("error", colorful::Color::Red), span)
                        .level_message("Empty begin")
                })?,

            _ => self.compile_function_call(list, span)?,
        })
    }

    fn compile(&mut self, value: Value<'a>) -> Result<BasicValueEnum> {
        Ok(match value {
            Value {
                data: ValueData::Integer(value),
                ty:
                    ValueType::Integer {
                        size: Some(size),
                        signed: Some(signed),
                    },
                ..
            } => self
                .context
                .custom_width_int_type(size)
                .const_int(value, signed)
                .as_basic_value_enum(),

            Value {
                data: ValueData::Integer(_),
                span,
                ..
            } => {
                return Err(Diagnostic::new(("error", colorful::Color::Red), span)
                    .level_message("Could not infer type for integer value"))
            }

            Value {
                data: ValueData::Function(_),
                ..
            }
            | Value {
                ty: ValueType::Function { .. },
                ..
            } => unreachable!(),

            Value {
                data: ValueData::String(s),
                ..
            } => self
                .context
                .const_string(&s, /*null_terminated:*/ false)
                .as_basic_value_enum(),

            Value {
                data: ValueData::Identifier(_),
                ..
            } => self.compile_identifier(value)?,

            Value {
                data: ValueData::List(_),
                ..
            } => self.compile_list(value)?,
        })
    }
}

/// Compile a value and return LLVM IR
pub fn compile(module_name: &str, value: Value<'_>) -> Result<String> {
    let mut compiler = Compiler::new(module_name);
    compiler.compile(value)?;
    Ok(compiler.module.print_to_string().to_string())
}
