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

## Parser

The parser is a basic `nom` parser with some error-handling stuff hand-rolled
on top of it. It's actually so hand-rolled that I'm thinking of just
hand-rolling the whole parser.

### Identifier


Identifiers may start with any of `[a-zA-Z_-]` and continue with any
of the former plus `[0-9]`.

Examples:
1. `foobar`
2. `-`
3. `foo+bar`
4. `_hello_i_am_dunder_`
5. `my-favorite-number-is-2+2`

### Integer

They are comprised of characters `[0-9]`.

They can be signed or unsigned.

They can have any size between 1 and 64.

Their size can be specified after their value or it can be inferred from
context.

Examples:
1. `0`
2. `42`
3. `42i32`
4. `42i42`
5. `37u21`

### String

Strings consist of any character except `"` inside a pair of `"`.

No escapes are currently supported.

Examples:
1. `""`
2. `"hello, world"`
3. `"ùnìcòdè is s↑pported"`

### List

Lists are made up of any amount of values, even zero, inside parenthesis.

They can be heterogenous.

Examples:
1. `()`
2. `(function main () i32 0)`
3. `((i love) (nested lists) 300)`

## Value

A value is a struct containing three fields:
1. `data`, containing what you might expect: A string content, an integer
   value, etc...
2. `ty`, representing the type of the value. This field is optional and will be
   inferred from context if not present.
3. `span`, which is the range of the input text which this value was parsed
   from. This is left-inclusive and right-exclusive.

Examples:
1. `Value { data: ValueData::Integer(0), ty: None, span: (0, 1) }`
2. `Value { data: ValueData::Integer(42), ty: Some(ValueType::Integer { size:
   32, signed: false }), span: (0, 2) }`

## Compiler

The Compiler is a very simple beast. It creates a LLVM Context, Builder and
Module upon instanciation.

### Frames

The Compiler contains internally a stack of `Frame`s, whose top value is the
first element of a `VecDeque<Frame>`.

Each `Frame` contains information about the current context being compiled,
such as variables currently defined in the scope.

For now, a new frame is created for every function.

### Identifier

Identifiers are compiled by looking up the corresponding variable in the frame
stack. Simple enough.

### Integer

Integers are compiled as LLVM integers.

### String

Integers are compiled as LLVM strings.

### List

Lists are compiled following this set of rules:

### function

If the head of the list is an Identifier with the contents `"function"`, a
new function definition is created in the global scope.

There exists no support for capturing variables (yet).

A function definition looks like this: `(function NAME ((PARAM_TYPE
PARAM_NAME)*) RETURN_TYPE BODY*)`

Type checking for a function definition involves checking that the last
expression in the body, which is the one that is returned by the function and
whill henceforth be referred to as the return value, has the type specified
in the function definition.

While type checking, if the return value has the data for an integer but no
type specified, and the function's return value is an integer type (e.g. `i32`,
`u64`, `i33`), the return value's type is set to the function's return value.
This allows definitions such as `(function main () i32 0)` to not re-state the
integer size.

Examples:
1. `(function main () i32 0)`

### otherwise

Otherwise, nothing is currently compiled and the compiler errors out.
