#![allow(dead_code)]

use std::collections::{hash_map::HashMap, vec_deque::VecDeque};

use crate::{
    diagnostic::Diagnostic,
    value::{Value, ValueData, ValueType},
};

use colorful::Color::Red;

#[derive(Debug)]
#[must_use]
enum Variable<'a> {
    ValueRef(&'a mut Value<'a>),

    // We utilize this enum variant for things such as functions, whose associated data depends on
    // compilation.
    Type(ValueType),
}

impl<'a> From<&'a mut Value<'a>> for Variable<'a> {
    fn from(value: &'a mut Value<'a>) -> Variable<'a> {
        Variable::ValueRef(value)
    }
}

impl From<ValueType> for Variable<'static> {
    fn from(ty: ValueType) -> Variable<'static> {
        Variable::Type(ty)
    }
}

pub struct Typechecker<'a> {
    // TODO: Look into using IndexMap
    variables: VecDeque<HashMap<&'a str, Variable<'a>>>,
}

type Result<T = (), E = Diagnostic> = std::result::Result<T, E>;

impl<'a> Typechecker<'a> {
    pub fn new() -> Self {
        Self {
            variables: VecDeque::from(vec![HashMap::new()]),
        }
    }

    fn pop_variable(
        &mut self,
        ident: &'a str,
        span: (usize, usize),
    ) -> Result<(usize, Variable<'a>)> {
        self.variables
            .iter_mut()
            .enumerate()
            .find_map(|(idx, frame)| frame.remove(ident).map(|value| (idx, value)))
            .ok_or_else(|| {
                Diagnostic::new(("error", Red), span)
                    .level_message(format!("Undefined variable {:?}", ident))
            })
    }

    // XXX: Can we ever have issues with `pop_variable` being called twice?
    fn expect_type(&mut self, expected_ty: &ValueType, value: &mut Value<'a>) -> Result {
        match value {
            Value {
                data: ValueData::Identifier(ident),
                span,
                ..
            } => {
                assert_eq!(value.ty, ValueType::Identifier);

                let (frame_idx, mut variable) = self.pop_variable(ident, *span)?;

                let ok = match variable {
                    Variable::ValueRef(ref mut value) => {
                        self.expect_type(expected_ty, &mut **value)?;
                        true
                    }

                    Variable::Type(ref ty) => *ty == *expected_ty,
                };

                self.variables[frame_idx].insert(ident, variable);

                if ok {
                    return Ok(());
                }
            }

            Value {
                data: ValueData::List(_),
                ..
            } => {
                self.typecheck_function_call(value, Some(expected_ty))?;

                return Ok(());
            }

            Value {
                data: ValueData::Integer(_),
                ty:
                    ty @ ValueType::Integer {
                        size: None,
                        signed: None,
                    },
                ..
            } => {
                if let ValueType::Integer { .. } = expected_ty {
                    *ty = expected_ty.clone();
                    return Ok(());
                }
            }

            // this covers strings and known-size integers
            Value { ty, .. } => {
                if *ty == *expected_ty {
                    return Ok(());
                }
            }
        }

        Err(
            Diagnostic::new(("error", Red), value.span).level_message(format!(
                "Expected type {:?}, found {:?}",
                expected_ty, value.ty,
            )),
        )
    }

    // TODO: factor this out cause the compiler wants it as well
    fn to_type(&mut self, value: &Value<'a>) -> Result<ValueType> {
        match &value.data {
            ValueData::Identifier(ident) => match ident.as_ref() {
                "i32" => Ok(ValueType::Integer {
                    size: Some(32),
                    signed: Some(true),
                }),

                _ => Err(Diagnostic::new(("error", Red), value.span)
                    .level_message(format!("Unknown type {:?}", ident))),
            },

            _ => Err(Diagnostic::new(("error", Red), value.span)
                .level_message("Expected a type".to_owned())),
        }
    }

    fn typecheck_function_call(
        &mut self,
        value: &mut Value<'a>,
        expected_return_ty: Option<&ValueType>,
    ) -> Result {
        let span = value.span;
        let mut list = variant_ref_mut!(value => List)?.iter_mut();

        match list.next() {
            Some(Value {
                data: ValueData::Identifier(ident),
                ..
            }) => {
                let (frame_idx, mut variable) = self.pop_variable(ident, span)?;

                if let Variable::ValueRef(Value {
                    ty:
                        ValueType::Function {
                            ref mut parameter_types,
                            ref mut return_type,
                        },
                    ..
                })
                | Variable::Type(ValueType::Function {
                    ref mut parameter_types,
                    ref mut return_type,
                }) = variable
                {
                    if expected_return_ty
                        .map(|expected_return_ty| *expected_return_ty != **return_type)
                        .unwrap_or(false)
                    {
                        return Err(Diagnostic::new(("error", Red), span).level_message(format!(
                            "Expected type {:?}, found {:?}",
                            expected_return_ty, return_type
                        )));
                    }

                    list.zip(parameter_types)
                        .map(|(param, ty)| self.expect_type(ty, param))
                        .collect::<Result>()?;
                } else {
                    unimplemented!() // FIXME: proper error
                }

                self.variables[frame_idx].insert(ident, variable);

                Ok(())
            }

            None => {
                return match expected_return_ty {
                    Some(expected_return_ty) => Err(Diagnostic::new(("error", Red), span)
                        .level_message(format!(
                            "Expected type {:?}, found the empty list",
                            expected_return_ty
                        ))),
                    None => Ok(()),
                }
            }

            // FIXME: Support inline functions and give a proper error for stuff that isn't meant
            // to be called
            _ => unimplemented!(),
        }
    }

    fn typecheck_function(&mut self, mut value: &mut Value<'a>) -> Result {
        let span = value.span;

        let list = variant_ref_mut!(&mut value => List)?;

        if list.len() <= 4 {
            return Err(Diagnostic::new(("error", Red), value.span)
                .level_message("Expected a function with a body".to_owned()));
        }

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                           ^^^^^^^^^^^
        let return_type = list
            .get(3)
            .ok_or_else(|| {
                Diagnostic::new(("error", Red), span)
                    .level_message("Expected a return type".to_owned())
            })
            .and_then(|value| self.to_type(value))?;

        // We know the list has a `last_mut` due to the check above.
        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                                           ^^^^^^^^^^^^
        let return_value = list.last_mut().unwrap();

        self.expect_type(&return_type, return_value)?;

        self.variables.push_front(Default::default());

        // Check the type for everything in the body, as well
        list.iter_mut()
            .skip(4)
            .map(|value| self.typecheck(value))
            .collect::<Result>()?;

        assert!(self.variables.pop_front().is_some());

        let name = variant_ref!(list.get(1).unwrap() => Identifier)?;
        self.variables.get_mut(0).unwrap().insert(
            name,
            Variable::Type(ValueType::Function {
                // FIXME: implement parameter types
                parameter_types: vec![],
                return_type: Box::new(return_type),
            }),
        );

        Ok(())
    }

    pub fn typecheck(&mut self, value: &mut Value<'a>) -> Result {
        match value.data {
            ValueData::Integer(..) | ValueData::Identifier(..) | ValueData::String(..) => Ok(()),

            ValueData::Function(_) => unreachable!(),

            ValueData::List(ref mut list) => {
                let head = if let Some(head) = list.get_mut(0) {
                    head
                } else {
                    return Err(Diagnostic::new(("error", Red), value.span)
                        .level_message("Empty list not supported".to_owned()));
                };

                match head.data {
                    ValueData::Identifier(ref ident) => match ident.as_ref() {
                        "function" => self.typecheck_function(value),
                        "begin" => variant_ref_mut!(value => List)?
                            .into_iter()
                            .skip(1)
                            .map(|value: &mut Value| self.typecheck(value))
                            .collect::<Result>(),
                        _ => self.typecheck_function_call(value, None),
                    },

                    _ => unimplemented!(),
                }
            }
        }
    }
}
