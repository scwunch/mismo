---
created: 2026-01-05 10:48
related:
  - "[[Mismo]]"
tags:
  - programming
---
# Memory Safety in Mismo
> This note was written after a [conversation with Gemini](https://g.co/gemini/share/4bcafb1ea8e1).  It is the current (as of #2026/Jan/7) iteration of Mismo's memory safety strategy, but it is still incomplete.  The bindings `box` and `ref` are excluded from this discussion, as they are "managed" bindings (either via GC or ref counting) and therefore do not impact overall memory safety.

> UPDATE: after another [conversation with Gemini](https://g.co/gemini/share/d0640948c2a3) about terminology, I've simplified and rewritten some of this.


## The Mode System 
In addition to a type system, *Mismo* also has an *access mode* for each value.  The mode of a value determines what you are allowed to do with that value.  The abilities includes:
- mutation of underlying value
- aliased or unique
- move to a field, buffer, or variable
- send to another thread

There is one mode for owned values (`var`) and three kinds of references, which are all shared pointers at runtime.
#### **`var`**
- owned, non-aliased value
- equivalent to Rust's owned values, or Pony's `iso`
- produced by literals and constructors
- can be "moved"
	- into a variable 
	- stored in a struct, enum, or container
	- returned from a function
- can be sent to another thread
- once moved or sent to another thread, the original variable is "unset" (and reading from it is a compiler error)
- if a `var` is aliased (or any part thereof), it is downgraded to a `mut!` for the duration of the lifetime of that borrow (compile-time guarantee)
#### **`mut!`**
- shared mutable borrow
- roughly equivalent to Pony's `ref`
- same capabilities as `var` except cannot be moved 
	- ie, can't be stored in containers, returned from function, or sent to other threads
- *Note: in an earlier version we considered renaming this mode to `dyn` as in "dynamic mutation".  We decided against that because `dyn` implies dynamic typing and vtables, which is wrong and unrelated.*
#### **`mut`**
- like `mut!` except limited only to changing fields that are copy types (ie, composed of primitives like `Int` and `Float`)
- `mut` is also restricted from changing the variant of an enum value if the enum has any variants with fields.
#### **`let`**
- shared immutable pointer
- equivalent to Rust's `&` or Pony's `box`

### Passing Modes
In Mismo, the transformation of values from one mode to another is governed by the access mode hierarchy.  `var` is at the top of the hierarchy, having the broadest capabilities, followed by `mut!`, then `mut`, and finally `let` is at the bottom, being the most restricted mode.

Or in type theory notation:
`var` <: `mut!` <: `mut` <: `let`

A higher mode can always be implicitly downgraded (cast) to a lower mode, but a lower mode cannot be promoted.  For example, a `mut!` may be passed into a function parameter expecting `let`, but a `let` cannot be passed as `mut`.  This directionality applies to passing arguments to functions as well as assigning to variables.

There are further restrictions on passing/sharing a `var` to maintain uniqueness and linearity.  If a `var` variable is moved into another `var` context (such as a field, container, `var` parameter, or another`var` variable) then it is consumed and considered "unset" at the original name.  Any attempt at reading from the old name is prohibited by the compiler at compile time until the variable is reset.  Fields cannot be "unset".

If a `var` is used in any context expecting `mut!`, `mut`, or `let`, it is *aliased* (no longer unique) and temporarily demoted to a `mut!` for the lifetime of all aliases, including aliases of its children.

```
var foo = Person("Ralph")
mut! name = foo.name
var bar = foo  -- this is an error since one borrow (name) is still live until the following line.
print(name)
```

> Note: this system was just recently simplified greatly.  Previously we had six "parameter passing conventions", three of which mapped onto the three reference modes, and the other three to `var`.  It was determined that the `inout` convention was not providing much value to the language while complicating the system, so it was removed, and the "copy" convention was relocated to another section of the documentation since it is such a specific outlier anyway.  This left us with just a one-to-one mapping for modes and "passing conventions" so we just use the term "mode" for both.

## Copy Types & Linearity 
Mismo types are divided into two universes: copy types and linear types.  All of the built-in primitive types are **copy types**.  They are:
- Nil 
- Bool
- UInt 
- Int 
- Float

Additionally, any compound type that is composed entirely of copy types is, by default, also a copy type.  For example:
```
struct Point
	.x Int
	.y Int 
	
enum Number
	Int(Int)
	Rational(Int, UInt)
	Float(Float)
	NaN
	Infinity 
```

A **linear type** on the other hand is any type that is either (a) explicitly marked as linear or (b) contains at least one linearly typed field.  

The only primitive type that is linear in Mismo is `Pointer[T]`.  So most linear types in the standard library (eg Array, String, Map) inherit their linearity by virtue of including a field of type `Pointer` (often several layers in) and include additional checks to ensure both memory safety and type safety.

There are also many unsafe functions on `Pointer`.  These functions may result in undefined behavior, or create copies of linear types, violating the guarantees of the type system.  However, these functions are useful for building efficient and safe types, FFI, and other low-level programming.

Copy types have a super power that linear types do not have: they can bypass the mode hierarchy by implicitly copying themselves.

Additionally, pass-by-implicit-copy is the default for copy types.  Given a function parameter `let foo: Type`, the mode is explicitly always `let`.  However, if the mode annotation is omitted, as in `foo: Type`, then if `Type` is linear, then the access mode will be `let`.  If `Type` is a copy type, then the implicit passing mode is "copy" and any argument passed to that parameter (regardless of access mode of the source) will be copied in, resulting in a `var` inside the body of the function.  If the linearity of `Type` depends on type arguments ( `Option[T]`, for example, is linear when `T` is `String`, but a copy type when `T` is `UInt`) then the access mode defaults to `let` as it has the broadest compatibility.

So whereas for a linear type, casting a `let` as a `var` is prohibited by the compiler, for copy types, this operation will copy the underlying value (bitwise, no function call) to make that transformation.  Moving down the mode hierarchy still works the same way for copy types and linear types.

> Note: when a copy typed value moves *down* the hierarchy (eg casting a `var` to a `mut`), the value is *aliased*.  When a value moves *up* the hierarchy, it is implicitly *copied*.

```
struct Point
	.x Int
	.y Int 
	
fn main():
	var p = Point(0,0)
	mut p_mut = p
	let p_let = p_mut
	var p2 = p_let  -- implicit copy happens here
	p_mut = Point(1,2)
	print(p_let)
	print(p2)
```
Output:
```
Point(1,2)
Point(0,0)
```

However, aliasing copy types is generally not necessary and not recommended.

## Why do we need so many modes?
I mean, Rust gets away with just three.  Pony has six, but they also include capabilities that we haven't discussed yet, namely thread sharing (`val` and `tag`).

So what are each of these modes bringing to the table?

Well, of course `var` is fundamental to this model.  

`let` is essential for cheaply sharing immutable data, and it's nice to be able to default to immutability.

`mut` and `mut!` give us shared mutability: a super power that is par for the course in garbage collected languages, but a dream for Rust developers and a nightmare for C developers.  It opens up powerful patterns.

### So why the difference between `mut` and `mut!`?
With great power comes great responsibility.  Shared mutability is known to cause issues with memory safety.  One of the main ways that happens is *the invalidation of references to the contents of dynamic containers*.

What is a dynamic container?  
- a value of a type that has the ability to perform  "shape mutation", ie, change layout.  Eg,
	- enum variants with differently typed payloads, 
	- types that can add/remove elements from a buffer, or 
	- types that may reallocate their contents.

How does this lead to invalid references?
- First obtain a mutable reference to a dynamic container
- Second, obtain a reference to an element of said container 
- Third, perform a "shape mutation" on the dynamic container 
	- eg, Array.push may reallocate, or Array.pop removes a value from a buffer
- Now our element reference may be pointing into memory that was moved, or to a value whose type has changed.  We have lost memory safety.

Mismo's solution is to have the compiler invalidate these references *at compile time*.  In order to do that, it must be able to track two things:
- the **provenance** of a reference 
	- ie, what field (or potential fields) the reference may belong to
- which references mutate values in a shape-unstable way

The compiler rejects a program when all three of these things are true:
- a shape-unstable mutation is made on mutable reference "a"
- "a" exists in the provenance chain(s) of "b"
- "b" is accessed after said mutation 

Thus, the distinction between `mut` and `mut!` helps the compiler track "shape mutations".

### What operations require `mut!`?
Most functions that require `mut!` only require it so they can pass it to other functions that require it.  There are actually only a small number of fundamental operations that require `mut!` (or `var`).  These operations all involve either moving a linear value, or changing the "shape" of a value.

1. Built-in Functions 
```
fn unsafe_load[T](mut! self: Pointer[T]) -> T
fn realloc[T](mut! self: Buffer[T], size: UInt)
fn free[T](mut! self: Buffer[T])
```

Note that `unsafe_store` is not mentioned here, as it technically only requires a `mut Pointer[T]` since setting the value behind a pointer doesn't necessarily drop any value.  However, many functions that use `unsafe_store` take `mut!` because they also use `unsafe_load` (like `Array.set`, which returns the old value) or `realloc` (like `Array.push`).

2. Assignment 
Furthermore, assignment (`foo = expression`) requires `foo` to be a `mut!` if `foo` is
- a linear type; OR
- any enum type except for a field-less enum.

Otherwise, this assignment only requires `mut`.

## Provenance Tracking
Within a function scope, tracking the provenance of a reference is not too hard.  You just need to look at how the references are generated (field access, or index of a buffer, or captured field of a matched enum value) and follow all branches of control flow.

Tracking provenance across function boundaries is tougher.  Since we are unwilling to do whole program compilation, each function must be checked only knowing the signatures of other functions.  This generally means the provenance of a referenced passed into a function as a parameter is unknown.

So what can we do?

### Mutual Exclusion Principle 
One option is what Swift does: **Mutual exclusion principle**.  That is, Swift must prove *at the callsite* of every function that references are non-overlopping.  That way you know that a reference passed in as a parameter will never be invalidated by a mutation because it's not possible that it points into that container.

The downsides of this approach are:
- puts undue burden on the user of the function
- prevents certain (safe) patterns where you *want* overlapping references 
- it's not always possible for the compiler to prove mutual exclusion, meaning you either have to give up or rely on runtime checks

This predicament could be ameliorated by provenance annotations (more on that later).

### Mutual Inclusion?
So Mismo instead leans the other way: The provenance of a let/mut parameter "a" is assumed to include all the other `mut!` parameters of the function that have the type of "a" in their children.  

For example, in the body of a function with signature `(mut! p: Person, let s: String)`, assuming the Person type has a field "friends" of type `Array[Person]`, and "name" of type `String`, then the provenance of "s" is assumed to be `[p.name, p.friends.*.name]` simply based on the type.  Even though the "s" argument at runtime may not be derived from "p", the compiler conservatively assumes it *might* be.

When building the provenance set, the compiler only looks at `mut!` mode parameters.  If it was `var` then we already know it's unaliased.  If it's `let` or `mut` then we know that shape mutation is impossible anyway so no need to track provenance in those cases.

Another quick example:
```
fn foo(let a: String,
       mut! b1: Array[String],
       mut! b2: Array[String],
       let c: Option[Int],
       mut! d: Option[Int]
      )
```
Provenance of
- "a" is `[b1.*, b2.*]`
- "b1" and "b2" are both `[b1, b2]` (they might alias one another )
- "c" is `[d]`
- "d" is empty, ie `[]` or `iso` (no possibility that it is contained by any other `mut!`)

This system is very conservative and may lead to many false positives.  That's where the last component comes in: **provenance annotations**.

### Provenance Annotations 
Unannotated references are assumed to have the broadest possible provenance.  Annotations narrow down the possibilities, allowing more mutation in the body of the function, and the cost is the compiler must then prove relative disjointness at the call site.  In other words, the programmer has the power to what degree the swift-like mutual exclusion principle is applied.

Here's what it looks like:
```
fn foo[T](mut! a1: Array[T], 
          mut! a1: Array[T], 
          let[a1.*] el: T,
         )
```

The provenance annotation `[a1.*]` means that the third parameter **may** alias any (that's the `.*`) field of `a1`.  And it **must not** alias `a2` as it is not included in the annotation.  If the annotation was `[a1.*, a2.*]` then it could alias elements from either array (this is also the default inferred provenance).  Notice that in either case, the annotation does not imply that the reference *must* alias the contents of one array of the other, only that it is *allowed* to.  It is always legal to pass in a completely isolated reference that does not alias any other parameter.

If you want to annotate a reference that cannot alias any other parameter, then `let[]` looks ugly so instead we write `iso let`.

## Too Many Modes?
4 modes is not that many, but the difference between `mut` and `mut!` is subtle enough to cause cognitive friction when writing function signatures.  So can we somehow simplify or compress this without losing the power?

We have some options.

### Eliminate `mut!`
This would probably just begin to feel like Rust without lifetime annotations, as now only `var` can perform shape-unstable mutations.  `mut` would still be useful, but the dream of shared mutability would be largely unrealized.  This would however simplify the language greatly, because without `mut!` there is also no need for provenance annotations.

> Update: Claude tried to convince me this is a good idea, but Gemini likes the original design.  [See conversation](https://gemini.google.com/share/8e7c48162916) (esp. last two messages).


### Eliminate `mut`
I mean, `mut` is hardly useful anyway, so why not get rid of it?  And then of course drop the `!` from `mut!`.  That would simplify the mental model (only one kind of mutation).  But it would lead to many more false positives (due to a relative overuse of `mut!`) and a greater reliance on provenance annotations (something I would like to keep to a minimum).

### Eliminate `let`
This would be sad to lose the "default immutable" idea, but it would help to simplify a bit.  Then we would rename `mut` and `mut!` to `let` and `mut` respectively.  So `let` would still be default, but now it would allow you to perform some limited mutation.  So "default *semi*mutable".

### Combine `mut` and `mut!`, compiler infers difference
This is perhaps the most obvious solution as the conceptual difference between them is not very clear.  However if we do this, then two things happen: 
- the function signature no longer tells you everything about the function contract; a core piece of information the provenance tracker uses is essentially hidden from the user until you look at the function body.
- the compiler complexity increases, the provenance checker has to first traverse the entire function call tree to determine which `mut`s are actually `mut!`s.

### Deemphasize `mut` by unnaming it
We could turn `mut` into the forgotten middle child by taking away its name and giving it back to `mut!` such that a `let` binding remains immutable, `mut` now allows all mutation, and then the default parameter passing convention (with no specified convention) would be this awkward anonymous semi-mutable mode.

If this was the case, how often would the developer actually use `let` in function signatures?  My guess is almost never.  That makes this proposal similar to the one of just eliminating `let`.

### Which option is best?
I don't really like any of them actually.  And even though I'm not happy with the complexity of so many modes, I'm inclined to keep them all as is.