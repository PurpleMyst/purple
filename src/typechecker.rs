#![allow(dead_code)]

use std::cell::{Ref, RefCell, RefMut};
use std::collections::{hash_map::HashMap, vec_deque::VecDeque};

use crate::{
    diagnostic::Diagnostic,
    value::{Value, ValueData, ValueType},
};

use colorful::Color::Red;

#[derive(Debug)]
struct Typechecker<'a> {
    variables: RefCell<VecDeque<HashMap<&'a str, ValueType>>>,
    types: RefCell<HashMap<(usize, usize), ValueType>>,
}

type Result<T = (), E = Diagnostic> = std::result::Result<T, E>;

pub(crate) fn parse_typename<'a>(value: &Value<'a>) -> Result<ValueType> {
    match &value.data {
        ValueData::Identifier("i32") => Ok(ValueType::Integer {
            size: Some(32),
            signed: Some(true),
        }),

        ValueData::Identifier(ident) => Err(Diagnostic::new(("error", Red), value.span)
            .level_message(format!("Unknown type {:?}", ident))),

        _ => {
            Err(Diagnostic::new(("error", Red), value.span)
                .level_message("Expected a type identifier"))
        }
    }
}

impl<'a> Typechecker<'a> {
    fn new() -> Self {
        Self {
            variables: RefCell::new(VecDeque::from(vec![HashMap::new()])),
            types: RefCell::new(HashMap::new()),
        }
    }

    fn get_variable(&self, value: &Value<'a>) -> Result<Ref<ValueType>> {
        let ident = variant!(value => Identifier)?;

        let variables = self.variables.borrow();

        // Due to not being able to return a `Result` from `Ref::map`, we must be sure that the
        // value returned in `Ref::map` is an `Ok`. We check before-hand with a separate loop to
        // return a pretty error instead of just panicking.
        variables
            .iter()
            .find(|level| level.contains_key(ident))
            .ok_or_else(|| {
                Diagnostic::new(("error", Red), value.span)
                    .level_message(format!("Undefined variable {:?}", ident))
                    .note("This error happened while type-checking")
            })?;

        Ok(Ref::map(variables, |variables| {
            variables.iter().find_map(|level| level.get(ident)).unwrap()
        }))
    }

    /// Return a mutable reference to a value's type through a shared reference.
    fn get_type(&self, value: &Value) -> RefMut<ValueType> {
        RefMut::map(self.types.borrow_mut(), |h| {
            h.entry(value.span).or_insert_with(|| value.ty.clone())
        })
    }

    fn expect_type(&self, expected_ty: &ValueType, value: &Value<'a>) -> Result {
        match value.data {
            ValueData::Identifier(_) => {
                if *self.get_variable(value)? == *expected_ty {
                    return Ok(());
                }
            }

            ValueData::List(_) => {
                let return_type = self.typecheck_function_call(value)?;
                if return_type == *expected_ty {
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
    /// Typecheck a non-builtin function call's parameters and return its return type
    fn typecheck_function_call(&self, value: &Value<'a>) -> Result<ValueType> {
        let span = value.span;
        let mut list = variant_ref!(value => List)?.iter();

        let ty = match list.next() {
            Some(
                value @ Value {
                    data: ValueData::Identifier(_),
                    ..
                },
            ) => self.get_variable(value)?,

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
            ref parameter_types,
            ref return_type,
        } = *ty
        {
            list.zip(parameter_types)
                .map(|(param, ty)| self.expect_type(ty, param))
                .collect::<Result>()?;

            // XXX: Can we avoid the clone here?
            Ok(*return_type.clone())
        } else {
            Err(Diagnostic::new(("error", Red), span)
                .level_message(format!("Expected a function, got {:?}", ty)))
        }
    }

    fn typecheck_function_definition(&self, value: &Value<'a>) -> Result {
        let list = variant_ref!(&value => List)?;

        if list.len() <= 4 {
            return Err(Diagnostic::new(("error", Red), value.span)
                .level_message("Expected a function with a body"));
        }

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //           ^^^^
        let name = variant_ref!(&list[1] => Identifier)?;

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                ^^^^^^^^^^
        let parameters = variant_ref!(&list[2] => List)?;

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                           ^^^^^^^^^^^
        let return_type = parse_typename(&list[3])?;

        // (function NAME PARAMETERS RETURN_TYPE ... RETURN_VALUE)
        //                                           ^^^^^^^^^^^^
        let return_value = list.last().unwrap();

        // Add the function to the global context, with just its type due to the value's data
        // depending on its compilation
        self.variables.borrow_mut().back_mut().unwrap().insert(
            name,
            ValueType::Function {
                parameter_types: parameters
                    .iter()
                    .map(|parameter| parse_typename(&variant_ref!(parameter => List)?[0]))
                    .collect::<Result<Vec<_>>>()?,

                return_type: Box::new(return_type.clone()),
            },
        );

        self.variables.borrow_mut().push_front(
            parameters
                .iter()
                .map(|parameter| {
                    let parameter = variant_ref!(parameter => List)?;
                    let parameter_ty = parse_typename(&parameter[0])?;
                    let parameter_name = variant!(&parameter[1] => Identifier)?;

                    Ok((parameter_name, parameter_ty))
                })
                .collect::<Result<_>>()?,
        );

        dbg!(&self.variables);

        self.expect_type(&return_type, return_value)?;

        // Typecheck the function's body
        list.iter()
            .skip(4)
            .map(|value| self.typecheck(value))
            .collect::<Result>()?;

        self.variables.borrow_mut().pop_front();

        Ok(())
    }

    fn typecheck(&self, value: &Value<'a>) -> Result {
        let span = value.span;

        match value.data {
            ValueData::Integer(..) | ValueData::Identifier(..) | ValueData::String(..) => Ok(()),

            ValueData::Function(_) => unreachable!(),

            ValueData::List(ref list) => {
                let head = list.get(0).ok_or_else(|| {
                    Diagnostic::new(("error", Red), span).level_message("Empty list not supported")
                })?;

                match head.data {
                    ValueData::Identifier("function") => self.typecheck_function_definition(value),

                    ValueData::Identifier("begin") => variant_ref!(value => List)?
                        .iter()
                        .skip(1)
                        .map(|value| self.typecheck(value))
                        .collect::<Result>(),

                    _ => self.typecheck_function_call(value).map(|_| ()),
                }
            }
        }
    }

    fn apply_types(&self, value: &mut Value<'a>) -> Result {
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
