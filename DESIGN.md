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

The parser supports the following constructs:
- Identifiers, which may start with any of `[a-zA-Z_-]` and continue with any
  of the former plus `[0-9]`.
- Numbers, which are comprised of characters `[0-9]` and can have a type
  specified right afterwards, the type being `[iu](\d+)`, and supporting any
  integer size between `1` and `64`, including odd sizes like `37`.
- Strings, which are any character except `"` between `"`. Escapes are not
  currently supported.
- Lists, which are sequences of other values between parenthesis. They are
  allowed to be heterogeneous

## Value

A value is a struct containing three fields:
1. `data`, representing basically a parsed version of whatever the user typed.
   This contains the string's contents, or the number's digits, etc...
2. `ty`, representing the type of the value. This can be present or can be not
   present. If it's not present, the value's type must be inferred from the
   context.
3. `span`, a `(usize, usize)` representing the portion of the input which this
   values take sup

## Compiler

The Compiler is a very simple beast. It creates a LLVM Context, Builder and
Module upon instanciation.

Compilation happens value-by-value, and each value type is compiled in the
following manner
- Identifiers are thought of as variables, looked up in what is known as the
  frame stack, going from the latest frame (first in the array) to the oldest
  frame (last in the array). The frame stack top is the beginning of the array
  so the iteration order is correct.
- Integers are compiled as simple LLVM integers, and the distinction between
  signed and unsigned is a bit lost in compilation.
- Strings are compiled as constant LLVM strings. Mutable strings are not
  supported yet, I guess.
- Lists are compiled based on their head
    1. If the list head is `Identifier("function")`, a function definition is
       created in the global context. (note: closures are not supported yet). A
       function definition takes the form of `(function NAME
       ((PARAM_TYPE PARAM)*) RETURN_TYPE BODY*)`. While parameters are not currently
       supported, the idea is that a new frame will be created where the
       arguments are assigned to variables. There is no support for Function
       values currently, but there will be in the future..
    2. In any other case, nothing is supported yet.

### Type Checking and Inference

Type inference, while not currently implemented, will be implemented following
a few dumb rules.

1. In a `function`, the last value must be of the type named in the function
   definition. This is useful for type inference of integer values in functions
   such as `main`
2. In a `define`, the right-hand-side's type must be assignable to the
   left-hand-side. The exact semantics of the preceding statement have not been
   worked out yet, such as how references are going to work et cetera.

