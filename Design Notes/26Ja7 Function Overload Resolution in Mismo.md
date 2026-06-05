---
created: 2026-01-07 23:22
related:
  - "[[Mismo]]"
  - "[[Function Overload Resolution in the Presence of Generics]]"
tags:
  - programming
---

Mismo does not support methods, only top-level functions.  However, methods can be emulated syntactically with UFCS and semantically with type-based overloading.  One function name may refer to many function implementations (overloads).  So when when the Mismo compiler encounters a function call like `foo.bar(arg)`, which implementation of `bar` is actually called?  And when generic functions are allowed to be called without specifying explicit type parameters, then the types of a given set of arguments may not be enough to disambiguate between overloads.

So this is the specification for how Mismo resolves calls to overloaded functions.

Given a function call `foo(arg1, arg2, ..., arg_n)` where `foo` is the name of the function, and `arg_i` is an expression, the compiler follows these steps:
1. Gather the set of *candidates*: all overloads of `foo`
	- `fn foo[T1,...TN](param1, ... param_n)`
2. Filter that list to only include *viable* candidates:
	- arity match: number of args equals number of params
	- type compatibility: for pair `(ta, tp)` of the type of the argument and the type of the parameter, `ta <: tp`
		- `a <: b` is true when:
			- `b` is a type variable; *OR*
			- the constructor of `a` is equal to the constructor of `b` ***and*** `ata <: bta` for each type argument
	- type arguments are inferred with no double-bindings
	- trait bounds implemented:
		- for the inferred type arguments, all required traits are implemented
3. Rank the list of viable candidates:
	- candidate A with parameter types `a1...an` is ranked higher than candidate B with parameter types `b1...b2` if: 
		- for *all* `i`: `ai <: bi` *and*
		- for *some* `i`: `ai` is more specific than `bi`
			- meaning : either `ai` has a concrete constructor whereas `bi` is generic, or the type arguments of `ai` are more specific than those of `bi`
4. If exactly one candidate is more specific than the rest, call that one.  Otherwise, raise an error.

### Examples
```
// simple type-based overload resolution of one argument
1. fn foo(s: String)
2. fn foo(i: Int)
foo("hi") => 1
foo(4) => 2
foo(true) => NoMatchError
foo() => NoMatchError

---
// direct ambiguity error
1. fn foo(a: Int)
2. fn foo(b: Int)
foo(5) => AmbiguousFnCallError

---
// specificity: concrete types beat type variables
1. fn foo[T](a: T)
2. fn foo(i: Int)
foo(63) => 2
foo("hi") => 1

---
// type variables can be bound by type arguments of arguments
1. fn foo[T](opt: Option[T], t: T)
2. fn foo(opt: Option[Int], i: Int)
3. fn foo(s: String)
foo(Option.Some("hello"), "world") => 1
foo(Option.Some("hello"), 55) => NoMatchError(double type arg bind)
foo(Option.Some(55), 0) => 2

---
// generics may lead to ambiguity in certain cases
1. fn foo[T](t: T, i: Int)
2. fn foo[T](i: Int, t: T)
foo(3, 5) => AmbiguousFnCallError(multiple "best" matches)
foo("hi", 1) => 1
foo(0, "hi") => 2

---
// Unbound type parameter — type var only appears in return type
fn foo[T]() -> T
foo() => UnboundTypeParamError

---
// Never argument gets no special treatment
fn foo(a: Int)
fn foo(a: String)
foo(panic("oh no"))  => NoMatchError  // Never ≠ Int, Never ≠ String

---
// Nested generic types for specificity
1. fn foo[T](a: Array[Array[T]])
2. fn foo(a: Array[Array[Int]])
foo([[1, 2], [3, 4]]) => 2   // specificity_gt recurses two levels deep
foo([["a", "b"]])     => 1

---
// Two type params — constrained vs unconstrained
1. fn foo[T, U](a: T, b: U)
2. fn foo[T](a: T, b: T)
foo(1, 2)     => AmbiguousError  // Var vs Var in both positions, neither beats other
foo(1, "hi")  => 1               // candidate 2 fails (T would bind to both Int and String)

---
// Fn type as argument
1. fn foo[T](f: Fn(T) -> T)
2. fn foo(f: Fn(Int) -> Int)
foo(|x: Int| x + 1)    => 2  // more specific
foo(|x: String| x)     => 1  // only match

---
// Tuple argument
1. fn foo[T](t: (T, Int))
2. fn foo(t: (String, Int))
foo(("hi", 5))  => 2
foo((true, 5))  => 1

---
// Zero-arity ambiguity (return type doesn't break the tie)
1. fn foo() -> Int
2. fn foo() -> String
foo() => AmbiguousError  // params are identical, return type not considered
```