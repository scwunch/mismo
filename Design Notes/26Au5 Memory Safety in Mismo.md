---
created: 2026-08-05 14:51
prev: "[[26Ja5 Memory Safety in Mismo]]"
related:
  - "[[Mismo]]"
---
# Memory Safety in Mismo
Alongside second-class references, and a linear/affine type system, the mode system is what allows Mismo to guarantee memory safety and type safety.  Like many modern languages, Mismo features linear or affine types for resources like memory.

The core insight of the model is tracking the **range** (potential pointees) of each borrowed reference, and tracking the mutation of **dynamic containers** (runtime-sized buffers and enums with differently typed payloads) in order to invalidate (at compile time) all references that may potential point to values in the mutated range.

This is a version of *group borrowing* (see [nmsmith](https://gist.github.com/nmsmith)/**[An alternative model for "lifetimes" in Mojo.md](https://gist.github.com/nmsmith/cdaa94aa74e8e0611221e65db8e41f7b)**).

## Linear Types

## Second-class References

## Borrow-Range Tracking

### Introduction
There are many ways that programming languages ensure memory safety.  

One of the foundational rules in Rust (and Rust-inspired languages) that the compiler uses to guarantee memory safety is the "mutable xor aliasable" rule for borrows.  This rule has been engrained deeply in the mind of some Rust developers to the degree that it seems synonymous with memory safety.  But it is not in fact strictly necessary.  In fact, Mismo allows any number of mutable references to coexist and mutate data concurrently and safely.  So what kinds of memory errors is this rule trying to prevent?

Essentially, if we allow multiple borrows of one value, where at least one of them is mutable, then it is possible to mutate that value in such a way that the other references are now dangling.  Of course it is also possible to mutate the value in such a way that the other references *do* remain valid.  So let's forget about the rule, and see if we can be more precise about how these errors occur, and how to prevent them.

### Dynamic Containers
We need to distinguish between different kinds of mutation.  Multiple pointers may mutate the value of an integer concurrently (within the same thread) without any safety issues.  This is **shape-stable mutation**, because although the *value* may change, the *size and type* of the value does not change.

Some values in Mismo actually do change their size and/or type at runtime.  These are called **dynamic containers**.  Mutation of these values, in some cases may constitute *moving a value* from one location to another.  The two archetypal dynamic containers are dynamic arrays and tagged unions.  The number and/or type of items stored in a dynamic container may change at runtime.

This is where we get dangling pointers from.  When a buffer shrinks in size, or is reallocated entirely, any pointers to its contents may now be invalid (it may still be physically in memory, but accessing it breaks the guarantees given to us by linearity).  Likewise, if a tagged union mutates from one variant to another, any pointers to the contents of the old value may be dangling, or worse, an entirely different type!

Mismo tracks precisely (at compile time) at what point in code dynamic containers are mutated in such a way that may invalidate pointers to its contents.

### Pointer Range Tracking
We say the **range** of a pointer is the set of possible locations (variables, fields, and buffers) that a pointer *may* point to at runtime.  Note that this is not usually a **contiguous** range, but rather a *set* of locations.  Mismo tracks the range of pointers at compile time.  

Within one function, this can be done simply by observing the provenance of pointers.  When the compiler encounters an expression such as `let r = a.foo`, then `r` is known to have the range `{a.foo}`.  If `r` is assigned to the result of a conditional, then it has the range that is the union of the ranges of the branches.  Eg, given `let r = if cond: a else b`, `r`  has range `{a, b}`.  If `a` and `b` are themselves references, then the range of `r` is the union of the ranges of `a` and `b`.

In a similar way, the range of pointers obtained through pattern matching is also deduced.

### Pointer Invalidation
How does this tracking help us maintain memory safety?  Now we can apply this rule: 
- whenever a reference is used in code that is to be executed *after* the mutation of a dynamic container included in the range of said reference, the compiler must reject the program with a diagnostic something like, "Cannot use pointer `r` here because it may have been invalidated by this mutation on line x."

That's it.  Now we are protected from dangling pointers and type-mismatched pointers.

### Tracking thru Function Boundaries
If Mismo had no functions, then what we have described so far would be enough.  But alas, there are a few programmers that are not too fond of writing entire programs with `fn main`, so we need to deal with the challenges of making this system consistent across function call boundaries.  

And since Mismo does not employ whole-program compilation for several reasons, tracking across function call boundaries does involve some extra annotations.  But don't worry, we do have some syntax sugar to make the common cases very lightweight.

#### Function Signatures
We've already learned the Mismo signatures include parameter names, types, and modes.  Now we will add two more features: 
- **mutation range:** a "mutates" clause 
- **parameter ranges:** range annotations for the provenance of references

Functions signatures look like this: 
```
fn name(mode{range} param: Type, ...) mutates {range} -> ReturnType
```

If a `{range}` is omitted, it means it is the empty range.  An empty mutation range should also omit the `mutates` keyword, and it means that the function performs no shape-unstable mutation observable to the caller.  An unannotated (empty) range for a parameter means that the range is wholly disjoint from the mutation range.

Here are some examples: 
1. Here's an example of a function that declares a mutation range: 
```
fn append[T](mut self: Array[T], var value: T) mutates {self.*} -> Nil
```

2. An example that swaps two arbitrary values
```
fn swap[T](mut{r2} a: T, mut{r1} b: T) mutates {r1: a, r2: b} -> Nil: 
	if __address_of(a) == __address_of(b): 
		return
	...
```

3. Appending to an array field of a struct containing two arrays
```
fn append_first[A, B](
	mut self: Lists[A, B], 
	let{r} ref_a: A,
	let ref_b: B
  ) mutates {r: self.first.*}
```

#### Syntactic Sugar
We have two punctuation marks we can put after `mut` or `let` to indicate the most commong things we want to communicate in our function signatures: 
- `mut!` (with the `!`) is sugar for the inclusion of the full parameter in the mutation range
- `let?`/`mut?` indicates the inclusion of the mutation range in the range of the parameter

Let's rewrite our three examples from above using the syntactic sugar: 
1. `fn append[T](mut! self: Array[T], val value: T): ...`
2. `fn swap[T](mut!? a: T, mut!? b: B): ...

The third example actually cannot be written precisely with the sugar.
```
fn append_first[A, B](
	mut! self: Lists[A, B],
	let? ref_a: A,
	let reg_b: B,
  ): ... 
```
is actually equivalent to 
```
fn append_first[A, B](
	mut self: Lists[A, B], 
	let{self} ref_a: A,
	let ref_b: B
  ) mutates {self: self}
```

Note that the mutation domain is now less precise: `self` instead of `self.lists.*` which means the caller will be forced to invalidate *all* references it has to the value passed as `self`, including references to `lists.second`, because the caller must assume that the entirety of `self` is mutated.

With the first two examples, there is no loss of precision using the sugar.