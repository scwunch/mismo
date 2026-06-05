---
created: 2026-03-22 21:43
related:
  - "[[Mismo]]"
---
table of contents
- philosophy: Why Mismo?
- Mode System
- Algebraic Data Types
	- structs and enums
	- struct of function-pointers rather than `dyn`
- control flow
	- UCS: "Ultimate Conditional Syntax"
- functions
	- function as methods
	- overload resolution
- implicits
- Closures & Coroutines
	- Capturing references
- concurrency
- modules

## Why Mismo?
Mismo is an exploration of mutable value semantics with pythonic syntax and a strong static algebraic type system.

Memory safety is guaranteed via scoped, second-class references, and the tools to make it both fast and expressive.

The syntax is indentation sensitive, supports UFCS (Uniform Function Call Syntax) and UCS (Ultimate Conditionsl Syntax).  Mismo takes inspiration from Python, Inko, and Ante.  

Mismo does **not** support:
- Garbage Collection 
- methods
- inheritance 
- traits / type-classes
- constraints on generic type parameters 
- first class references (storing references in structs, returning references from functions)

Even so, Mismo aims to support:
- memory safety
	- affine types, mode system
- flexible data modeling 
	- algebraic data types, 
- powerful, ergonomic generic programming 
	- function overloading, implicits

## The Mode System 
Mismo uses Mutable Value Semantics.  

Each binding (variables and function parameters) has an associated *access mode*.  The mode of a binding determines what you are allowed to do with that value.  The set of possible abilities includes:
- mutation of underlying value
- aliasing 
- moving to a field, buffer, or variable
- sending to another thread

There is one mode for owned values (`var`) and three kinds of references, which are all shared pointers at runtime.
#### **`var`**
- owned, unique (non-aliased) value
- equivalent to Rust's owned values, or Pony's `iso`
- produced by literals and constructors
- can be "moved"
	- into a variable 
	- stored in a struct, enum, or container
	- returned from a function
	- sent to another thread
#### **`mut!`**
- shared mutable borrow
- roughly equivalent to Pony's `ref`
- same capabilities as `var` except cannot be moved 
	- ie, can't be stored in containers, returned from function, or sent to other threads
- can *move* a value, but only via transitive assignment (ie, no *sink*)
#### **`mut`**
- like `mut!` except limited only to **shape-stable** mutation (usually just fields which are primitives like `Int` and `Float`)
- `mut` is also restricted from changing the variant of an enum value if the enum has variants with differently typed fields.
#### **`let`**
- shared immutable reference 
- equivalent to Rust's `&` or Pony's `box`

### Passing Modes
In Mismo, the transformation of values from one mode to another is governed by the access mode hierarchy.  `var` is at the top of the hierarchy, having the broadest capabilities, followed by `mut!`, then `mut`, and finally `let` is at the bottom, being the most restricted mode.

Or in type theory notation:
`var` <: `mut!` <: `mut` <: `let`

A higher mode can always be implicitly downgraded (cast) to a lower mode, but a lower mode cannot be promoted.  For example, a `mut!` may be passed into a function parameter expecting `let`, but a `let` cannot be passed as `mut`.  This directionality applies to passing arguments to functions as well as assigning to variables.

There are further restrictions on passing/sharing a `var` to maintain uniqueness and linearity.  If a `var` variable is moved into another `var` context (such as a field, container, `var` parameter, or another`var` variable) then it is consumed and considered "unset" at the original name.  Any attempt at reading from the old name is prohibited by the compiler at compile time until the variable is reset.  Fields cannot be "unset".

If a `var` is used in any context expecting `mut!`, `mut`, or `let`, it is *aliased* (no longer unique) and temporarily demoted to a `mut!` for the lifetime of all its aliases, including aliases of its children.  This guarantees that a value is never moved out from under an alias and is enforced at compile time.

```
var foo = Person("Ralph")
mut! name = foo.name
var bar = foo  -- this is an error since one borrow (name) is still live until the following line.
print(name)
```

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

`realloc` and `free` require `mut!` because they potentially invalidate any shared pointers by freeing or moving memory.  `unsafe_load` requires `mut!` because, even though it does not physically free memory, conceptually the value has been moved out of the pointer and into the caller's context, which also invalidates any shared pointers to the previous location.  This helps us guarantee we do not obtain two copies of a unique value.

Note that `unsafe_store` is not mentioned here.  It technically only requires a `mut Pointer[T]` since setting the value behind a pointer doesn't necessarily drop any value—eg initializing a null pointer.  However, many functions that use `unsafe_store` take `mut!` because they also use `unsafe_load` (like `Array.set`, which returns the old value) or `realloc` (like `Array.push`).  

2. Assignment 
Furthermore, assignment (`foo = expression`) requires `foo` to be a `mut!` if `foo` is
- a linear type; OR
- any enum type except for a field-less enum.

Otherwise, this assignment only requires `mut`.

## Provenance Tracking
Within a function scope, tracking the provenance of a reference is not too hard.  You just need to look at how the references are generated (field access, or index of a buffer, or captured field of a matched enum value) and follow all branches of control flow.

Tracking provenance across function boundaries is tougher.  Since we are unwilling to do whole program compilation, each function must be checked only knowing the signatures of other functions.  This generally means the provenance of a referenced passed into a function as a parameter is unknown.

So what can we do?



## Memory Safety thru Modes
Mismo ensures memory safety through a **Mode System** that tracks how values are accessed, moved, and shared. By combining this with a distinction between **Copy** and **Linear** types, the compiler can guarantee memory safety, thread safety and prevent common pointer errors without requiring a heavy runtime.

### 1. The Mode System
Every value in Mismo has an **access mode** that defines its capabilities, such as whether it can be mutated, moved, or sent across threads. These modes exist in a hierarchy where higher modes can be implicitly downgraded (cast) to lower ones.

$$var <: mut! <: mut <: let$$

| **Mode**   | **Description**                       | **Capabilities**                          |
| ---------- | ------------------------------------- | ----------------------------------------- |
| **`var`**  | The unique, primary owner of a value. | Moveable, thread-safe, full mutation.     |
| **`mut!`** | A shared mutable reference.           | Full mutation; cannot be moved or stored. |
| **`mut`**  | A restricted mutable reference.       | Can only mutate primitive "copy" fields.  |
| **`let`**  | A shared immutable reference.         | Read-only access.                         |

#### Ownership and Unsetting:
When a `var` is moved into a new variable or container, the original name is "unset." Any attempt to read from an unset variable results in a compile-time error. If a `var` is borrowed as a reference, it is temporarily demoted to `mut!` until that borrow expires, ensuring no two parts of the code have conflicting views of the data.

---

### 2. Copy Types & Linearity
Mismo categorizes all types into two "universes" that determine how they behave during assignment and function calls.

- **Copy Types:** These include primitives (e.g., `Int`, `Float`, `Bool`) and any compound types (structs/enums) composed entirely of other copy types.    int
    - **Implicit Copy:** When a copy type is passed to a context requiring a higher mode (like `let` to `var`), the compiler performs a bitwise copy automatically.        
- **Linear Types:** Any type marked as linear or containing a linear field (like `Pointer[T]`).    
    - **Strict Linearity:** These cannot be copied implicitly. They must be moved or borrowed according to the mode hierarchy to ensure memory is managed correctly.        

---

### 3. Shared Mutability and Shape Safety
Mismo provides multiple modes to balance the flexibility of shared pointers with the requirements of memory safety. The distinction between `mut` and `mut!` is specifically designed to handle **Shape Mutation**.

**The Danger of Shape Mutation:**
A "shape-unstable" mutation occurs when a container changes its layout—for example, an `Array` reallocating its buffer or an `Enum` changing variants. If one reference modifies the shape while another reference points to an internal element, the second reference becomes invalid (a "dangling pointer").

**How Mismo Prevents Invalidation:**
- **`mut` (Safe Sharing):** Allows changing data values (like an `Int` field) but forbids changing the "shape" of the object. This is always safe to share.    
- **`mut!` (Controlled Power):** Allows shape-altering operations (like `realloc` or `free`).    
- **Provenance Tracking:** The compiler tracks the "origin" of references. If you perform a `mut!` operation on a container, the compiler automatically invalidates any other active references derived from that container at compile time.
    

## Algebraic Data Types
Mismo supports product types (`struct`) as well as sum types (`enum`) and pattern matching.

Mismo does not support dynamic; all types in Mismo are known at compile time.  However, we can simulate runtime dynamic types by making a struct of function pointers.

## Control Flow
### Ultimate Conditional Syntax

## Function Overloading
Mismo does not support methods on types.  Rather, it leans into type-based overloading of top-level functions.

### Function Overload Resolution
### Mutual Exclusion of Overloads
The compiler must guarantee by static analysis of the signatures of functions (without looking at any call sites) that no ambiguous function call is possible.  

## Continuation Passing Style, Closures and Coroutines
References are strictly second-class in Mismo
—they cannot be stored in structs, containers, or returned from functions. This means that functions that would ordinarily be expected to return a reference, like `Array.get` must now be written to accept a closure. In Mismo, the language I'm designing, this looks like this:

```
fn at[T, R](mut! self: Array[T], index: Nat, some: fn(mut! T)->R, none: fn()->R) -> R:
	if index < self.size:
		some(self.buffer.deref_mut(index))
	else:
		none()


fn at[T, R](mut! self: Array[T], index : Nat, do(mut! T)->R) -> Option[R]:
	if index < self.size:
		Option.Some(do(self.buffer.deref_mut(index))
	else:
		Option.None

  
fn main():
	var array = make_array()
	array.at(5, 
		|val|: 
			print("sixth element is {val}")
		||: 
			print("index 5 out of range")
	)
	array.at(5): 
		some(elem): print("sixth element is {elem}")
		none(): print("index 5 out of range")
		
	array.at(5
		some(elem): print("sixth element is {elem}")
		none(): print("index 5 out of range")
	)
	
	array.at(5, |elem|: 
		print("sixth element is {elem})
	).or_else(||: 
		print("index 5 out of range")
	)
```

This program will print the value of the sixth element if it exists, otherwise will print the "out of range" message.

Note:
- the function is renamed to `at` (instead of `get`) to better reflect its use
- `mut!` indicates the borrow type; arguments are passed as mutable references in this case
- the parameters `some` and `none` are both closures, but named like so to reflect the fact that they are emulating a kind of option enum.

In a language like Rust, `Option<&T>` is a piece of data that can be moved around, which is why it requires complex lifetime tracking to ensure the `&T` doesn't outlive the `Array`.

In **Mismo**, the CPS style bypasses this complexity:
1. **Scope-Limiting:** The `val` (the mutable reference) only exists within the stack frame of the `some` closure. Once that closure returns, the reference is physically gone. It _cannot_ escape because Mismo forbids storing it in a struct or returning it.
2. **Zero-Cost Abstraction:** If the compiler inlines these closures, this should compile down to the exact same machine code as a standard C pointer access, but with 100% memory safety.

## Implicits
Mismo does not feature traits of type-classes or interfaces.  There is no way to directly constrain what types may be passed to a given type parameter.  Therefore, the only way to perform any operation (safely) on such types is to pass in the operation to be performed as an argument.

## Concurrency
Actor Model

## Modules

## Appendix
Point to other resources for...
- syntax
- details of provenance checker
- patterns and anti-patterns
	- passing closures as arguments
	- storing references
	- iterators

