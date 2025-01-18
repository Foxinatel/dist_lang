# Language specification

This document contains the current specification for the language.

## Literals

- The unit literal is expressed as `<>`
- Integer literals are expressed as common arabic numerals
- Boolean literals are `true` and `false`
- The nil literal (an empty list) is expressed as `T{}`, where `T` is the inner type of the list
    - e.g. `Int{}` is a list of integers, or `[Int -> Int]{}` is a list of boxed functions from Int to Int
    - Note: Writing the type in the expression is done to simplify type inference. This may be removed in future if stronger type inference is added to the language.

## Numeric operations

These operations take an `Int` expression on either side and return an `Int` value:
- `+` addition
- `-` subtraction
- `*` multiplication
- `/` division

These operations take an `Int` expression on either side and return a `Bool` value:
- `==` equality
- `!=` inequality
- `>` greater than
- `<` less than
- `>=` greater than or equal
- `<=` less than or equal

## Variables

This language contains two types of binding, as it is inspired by the dual-context lambda calculus.

### Regular
Regular binding is expressed as follows:

```
let <var> = <expr> in <body>
```
(e.g. `let n = 25 in (fib n)`)

### Mobile
The second type of binding is for mobile binding, and takes the form:
```
let box <var> = <expr> in <body>
```

A condition of this type of binding is that `<expr>` must be of a mobile type. This type of binding will run the boxed expression of `<expr>` in parallel with `<body>`.

While executing `<body>`, if we encounter a reference to `<var>`, then the execution of the body will be blocked until the bound boxed expression has finished being computed.

This allows for improved parallelism in cases such as:

```
let box a = box <expensive_1> in
let box b = box <expensive_2> in
a + b
```
As long as `<expensive_2>` does not depend on `a`, both of these bindings will run in parallel with each other, thus giving strong performance improvements with very little cognitive overhead.

## Types

Types are defined as the following:

```
T ::= <>
    | Bool
    | Int
    | [T]
    | {T}
    | T -> T
```

`[T]` represents a "mobile" type. In Blues this was expressed as `[]T`, however this has been changed to lessen ambiguity around types such as `[]Int -> Int`.

`{T}` represents a list of values of type `T`.

## Branching

Branching can be performed through the following syntax:

```
if <cond> then <if_true> else <if_false>
```

Here, `<cond>` must be of type `Bool`, and the types of `<if_true>` and `<if_false>` must match.

## Functions

### Regular

Regular functions are written with a Rust-like syntax.

```
|<binding>:<type>|(<body>)
```

For example, the identity function for the type `Int` would be `|x: Int|(x)`.

### Fixed Point

Fixed-point functions are also available, through a similar syntax:
```
fix |<binding>:<type>|(<body>)
```

For this to be valid `<type>` must be of some function type.

## Lists

There are two primary operations that can be performed on lists: Appending and indexing.

To append to a list:
```
<list> :: <item>
```

For this to be valid, `<list>` must be an expression of type `{T}`, and `<item>` must be an expression of type `T`.

To index into a list:
```
<list>[<index>]
```

For this to be valid, `<list>` must be an expression of type `{T}`, and `<index>` must be an expression of type `Int`. The returned value will be of type `T`.