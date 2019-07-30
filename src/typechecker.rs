#![allow(dead_code)]

use std::cell::{RefCell, RefMut};
use std::collections::{hash_map::HashMap, vec_deque::VecDeque};

use crate::{
    diagnostic::Diagnostic,
    value::{Value, ValueData, ValueType},
};

use colorful::Color::Red;

#[derive(Debug)]
#[must_use]
enum Variable<'a> {
    ValueRef(&'a Value<'a>),

    // We utilize this enum variant for things such as functions, whose associated data depends on
    // compilation.
    Type(ValueType),
}

impl<'a> Variable<'a> {
    fn ty(&self) -> &ValueType {
        match self {
            Variable::ValueRef(Value { ty, .. }) | Variable::Type(ty) => ty,
        }
    }
}

#[derive(Debug)]
struct Typechecker<'a> {
    variables: VecDeque<HashMap<&'a str, Variable<'a>>>,
    types: RefCell<HashMap<(usize, usize), ValueType>>,
}

type Result<T = (), E = Diagnostic> = std::result::Result<T, E>;

impl<'a> Typechecker<'a> {
    fn new() -> Self {
        Self {
            variables: VecDeque::from(vec![HashMap::new()]),
            types: RefCell::new(HashMap::new()),
        }
    }

    fn get_variable(&self, value: &Value<'a>) -> Result<&Variable<'a>> {
        let ident = variant_ref!(value => Identifier)?;

        self.variables
            .iter()
            .find_map(|level| level.get(ident))
            .ok_or_else(|| {
                Diagnostic::new(("error", Red), value.span)
                    .level_message(format!("Undefined variable {:?}", ident))
            })
    }

    /// Return a mutable reference to a value's type through a shared reference.
    fn get_type(&self, value: &Value) -> RefMut<ValueType> {
        RefMut::map(self.types.borrow_mut(), |h| {
            h.entry(value.span).or_insert_with(|| value.ty.clone())
        })
    }

    fn expect_type(&self, expected_ty: &ValueType, value: &Value<'a>) -> Result {
        match value.data {
            ValueData::Identifier(_) => match self.get_variable(value)? {
                Variable::ValueRef(value) => {
                    self.expect_type(expected_ty, value)?;
                    return Ok(());
                }

                Variable::Type(ref ty) => {
                    if *ty == *expected_ty {
                        return Ok(());
                    }
                }
            },

            ValueData::List(_) => {
                let return_type = self.typecheck_function_call(value)?;
                if return_type == expected_ty {
                    return Ok(());
                } else {
                    return Err(Diagnostic::new(("error", Red), value.span).level_message(
                        format!(
                            "Expected return type {:?}, found {:?}",
                            expected_ty, return_type,
                        ),
                    ));
                }
            }

            ValueData::Integer(_) => {
                let mut ty = self.get_type(value);

                if let ValueType::Integer {
                    size: None,
                    signed: None,
                } = &*ty
                {
                    if let ValueType::Integer { .. } = expected_ty {
                        *ty = expected_ty.clone();
                        return Ok(());
                    }
                } else if *ty == *expected_ty {
                    return Ok(());
                }
            }

            ValueData::String(_) | ValueData::Function(_) => {
                if *self.get_type(value) == *expected_ty {
                    return Ok(());
                }
            }
        }

        Err(
            Diagnostic::new(("error", Red), value.span).level_message(format!(
                "Expected type {:?}, found {:?}",
                expected_ty,
                self.get_type(value),
            )),
        )
    }

    // TODO: factor this out cause the compiler wants it as well
    fn to_type(&self, value: &Value<'a>) -> Result<ValueType> {
        match &value.data {
            ValueData::Identifier("i32") => Ok(ValueType::Integer {
                size: Some(32),
                signed: Some(true),
            }),

            ValueData::Identifier(ident) => Err(Diagnostic::new(("error", Red), value.span)
                .level_message(format!("Unknown type {:?}", ident))),

            _ => Err(Diagnostic::new(("error", Red), value.span)
                .level_message("Expected a type identifier")),
        }
    }

    /// Typecheck a non-builtin function call's parameters and return its return type
    fn typecheck_function_call(&self, value: &Value<'a>) -> Result<&ValueType> {
        let span = value.span;
        let mut list = variant_ref!(value => List)?.iter();

        let ty = match list.next() {
            Some(
                value @ Value {
                    data: ValueData::Identifier(_),
                    ..
                },
            ) => self.get_variable(value)?.ty(),

            // TODO: Support inline functions
            Some(value) => {
                return Err(Diagnostic::new(("error", Red), span).level_message(format!(
                    "Expected a callable, found value of type {:?}",
                    self.get_type(value)
                )))
            }

            None => {
                return Err(Diagnostic::new(("error", Red), span)
                    .level_message("Expected a function call, found the empty list"))
            }
        };

        if let ValueType::Function {
            parameter_types,
            return_type,
        } = ty
        {
            list.zip(parameter_types)
                .map(|(param, ty)| self.expect_type(ty, param))
                .collect::<Result>()?;

            Ok(return_type)
        } else {
            Err(Diagnostic::new(("error", Red), span)
                .level_message(format!("Expected a function, got {:?}", ty)))
        }
    }

    fn typecheck_function_definition(&mut self, mut value: &mut Value<'a>) -> Result {
        let list = variant_ref_mut!(&mut value => List)?;

        if list.len() <= 4 {
            return Err(Diagnostic::new(("error", Red), value.span)
                .level_message("Expected a function with a body"));
        }

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                           ^^^^^^^^^^^
        let return_type = self.to_type(&list[3])?;

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                                           ^^^^^^^^^^^^
        let return_value = list.last_mut().unwrap();

        self.expect_type(&return_type, return_value)?;

        self.variables.push_front(Default::default());

        // Typecheck the function's body
        list.iter_mut()
            .skip(4)
            .map(|value| self.typecheck(value))
            .collect::<Result>()?;

        self.variables.pop_front();

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //           ^^^^
        let name = variant_ref!(&list[1] => Identifier)?;

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                ^^^^^^^^^^
        let parameters = variant_ref!(&list[2] => List)?;
        let parameter_types = vec![];

        if !parameters.is_empty() {
            return Err(Diagnostic::new(("error", Red), list[2].span)
                .level_message("Parameters are not supported"));
        }

        // Add the function to the global context, with just its type due to the value's data
        // depending on its compilation
        self.variables[0].insert(
            name,
            Variable::Type(ValueType::Function {
                parameter_types,
                return_type: Box::new(return_type),
            }),
        );

        Ok(())
    }

    fn typecheck(&mut self, value: &mut Value<'a>) -> Result {
        let span = value.span;

        match value.data {
            ValueData::Integer(..) | ValueData::Identifier(..) | ValueData::String(..) => Ok(()),

            ValueData::Function(_) => unreachable!(),

            ValueData::List(ref mut list) => {
                let head = list.get_mut(0).ok_or_else(|| {
                    Diagnostic::new(("error", Red), span).level_message("Empty list not supported")
                })?;

                match head.data {
                    ValueData::Identifier("function") => self.typecheck_function_definition(value),

                    ValueData::Identifier("begin") => variant_ref_mut!(value => List)?
                        .into_iter()
                        .skip(1)
                        .map(|value: &mut Value| self.typecheck(value))
                        .collect::<Result>(),

                    _ => self.typecheck_function_call(value).map(|_| ()),
                }
            }
        }
    }

    fn apply_types(&mut self, value: &mut Value<'a>) -> Result {
        value.ty = self.get_type(&*value).to_owned();

        if let ValueData::List(ref mut vs) = value.data {
            vs.iter_mut()
                .map(|value| self.apply_types(value))
                .collect::<Result>()?;
        }

        Ok(())
    }
}

pub fn typecheck<'a>(value: &mut Value<'a>) -> Result {
    let mut typechecker = Typechecker::new();
    typechecker.typecheck(value)?;
    typechecker.apply_types(value)?;
    Ok(())
}
