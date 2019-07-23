# purple

A simple language that's more an exercise in language design than
anything else

It's a LISP-y language due to my appreciation of them; I couldn't find a
language that fit my needs and had an active community so I decided to make my
own

This language has a few design goals:

- Simple to use! It shouldn't need to be learnt _much_
- Good interop with the C ABI to be able to leverage existing libraries such as
  libcurl.
- Safety. It should be hard to shoot yourself in the foot.
- Performant functional programming

## Language

The current syntax is very basic; There exist the following values:

- `Value::Number(u64)` → 64-bit integer
- `Value::Identifier(&'_ str)` → identifier, refers to variables, right now
  just alphabetical characters
- `Value::String(String)` → strings! no escapes supported just yet
- `Value::SExpr(Vec<Value<'_>>)` → s expressions, not much to see here: `(a b)`

## Ideas

### define

Used to assign variables, but they can only be assigne donce

- `(define $lhs:Identifier $rhs:Value)` → Assign `$lhs` to `$rhs`, type is
  inferred from `$rhs`

### lambda

Used to create anonymous functions

- `(lambda (($arg_ty:Identifier $arg:Identifier)*) $ret_ty:Identifier $body:Value*)`

Example: `(lambda ((u64 x)) u64 (* 2 x))`

### function

Same as lambda, except there is a `$name:String` argument. This must be used to
define `main`.

Example: `(function main () u32 0)`
