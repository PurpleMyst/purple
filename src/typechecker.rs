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

    fn get_variable(&self, ident: &'a str, span: (usize, usize)) -> Result<&Variable<'a>> {
        self.variables
            .iter()
            .find_map(|level| level.get(ident))
            .ok_or_else(|| {
                Diagnostic::new(("error", Red), span)
                    .level_message(format!("Undefined variable {:?}", ident))
            })
    }

    fn get_type(&self, value: &Value) -> RefMut<ValueType> {
        RefMut::map(self.types.borrow_mut(), |h| {
            h.entry(value.span).or_insert_with(|| value.ty.clone())
        })
    }

    fn expect_type(&self, expected_ty: &ValueType, value: &Value<'a>) -> Result {
        match value {
            Value {
                data: ValueData::Identifier(ident),
                span,
                ..
            } => {
                assert_eq!(value.ty, ValueType::Identifier);

                match self.get_variable(ident, *span)? {
                    Variable::ValueRef(value) => {
                        self.expect_type(expected_ty, value)?;
                        return Ok(());
                    }

                    Variable::Type(ref ty) => {
                        if *ty == *expected_ty {
                            return Ok(());
                        }
                    }
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
                    ValueType::Integer {
                        size: None,
                        signed: None,
                    },
                ..
            } => {
                if let ValueType::Integer { .. } = expected_ty {
                    *self.get_type(value) = expected_ty.clone();
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
        &self,
        value: &Value<'a>,
        expected_return_ty: Option<&ValueType>,
    ) -> Result {
        let span = value.span;
        let mut list = variant_ref!(value => List)?.iter();

        match list.next() {
            Some(Value {
                data: ValueData::Identifier(ident),
                ..
            }) => {
                let variable = self.get_variable(ident, span)?;

                if let ValueType::Function {
                    ref parameter_types,
                    ref return_type,
                } = variable.ty()
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

    fn typecheck(&mut self, value: &mut Value<'a>) -> Result {
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
