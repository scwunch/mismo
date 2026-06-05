---
created: 2024-10-30 20:35
related:
  - "[[Pili]]"
---
> [!NOTE]
> Much of this is out-of-date with the current state of the design of Mismo.

## Introduction
## Why Mismo?
## Basic Types
The `Nil` type has one value, `nil` which stands for "no usable value".

The `Bool` type has two values: `true` and `false`.

### Numeric Types
`Byte` is an 8-bit unsigned integer
`Nat` is a 64-bit unsigned integer
`Int` is a 64-bit signed integer
`Float` is a 64-bit floating point number


> [!IDEA] Considering
> Mismo will likely eventually support multiple bit-widths for numeric types (eg, UInt16, Float32, Int128, etc).  When that happens, a number-tower-like hierarchy will also likely be implemented. 
 Traits will be introduced like `Nat`, `Int`, and `Real`, and `Number` where all numeric types would implement `Number`, float types would implement `Real`, and uint types would implement all those traits.  Generic functions can then be written to support whatever numeric types are appropriate, without writing many overloads.
 > 
> Furthermore, this will be made more ergonomic if I implement the shorthand syntax for generics.

### Strings
Strings are UTF-8 strings.  Strings are, like all values in Mismo, immutable by default, but they can be mutated when needed.

Strings literals use 'single quotes' or "double quotes", and both may contain escape characters such as `\n`, `\t`, or `\u{235}`.

Multi-line strings are supported by enclosing triple (or more) single or double quotes.  The beginning triple quote must be followed by a newline (which is not counted as part of the string contents).  The end triple quote must be on the start of the newline, except for leading spaces which determines the indentation of the whole string literal.  That is, each line of the multi-line string has as many spaces trimmed from the beginning as the indentation of the ending triple quote.  

Examples:
```
let haiku = """
    Let me die in spring
      beneith the cherry blossoms
        while the moon is full.
    """
haiku == "Let me die in spring\n  beneath the cherry blossoms\n    while the moon is full."
```

The following are syntax errors:
```
let not_multi = """this doesn't look like a multi-line string"""

let wrong = """ oops, this should be 
	on the next line! 
	"""

let also_wrong = """
	so far so good...
	but this can't be here ->"""
```

"Raw" strings are enclosed in single or double back-ticks.  Escape sequences and interpolation are not supported in raw strings.

So, for example, 
```
`\w+\n`  ==  "\\w+\\n"
  and
  ``this one has `backticks` inside`` == "this one has `backticks` inside"
```

Raw multi-line strings combine the rules from raw string literals as well as multi-line string literals.  For example:
`````
let reference = ```
	newline = \n
	tab = \t
	triple-quotes = """
	```
let my_markdown_syntax_example = ````
	In markdown syntax, you can encase code in "code-fences" like this:
	```python
	variable = "hello\nworld!"
	```
	````
`````

## Bindings
Local variables in Mismo can be declared or "bound" in exactly five different ways.  These are Mismo's five **bindings**.

`let`-bound variables are read-only projections.  `let` is also the default parameter passing convention, since all bindings can be passed as `let`.  If you have a `let` variable, you can make aliases of it or any of its fields without limiting usage of the original variable.

| Binding | Allocation         | Mutability | Aliasing | Thread-safe | Storable |
| ------- | ------------------ | ---------- | -------- | ----------- | -------- |
| `var`   | inline             | ✅ yes      | ❌ no     | ✅ yes       | ✅ yes    |
| `mut`   | (pointer)          | ✅ yes      | ✅ yes    | ❌ no        | ❌ no     |
| `let`   | (pointer)          | ❌ no       | ✅ yes    | ❌ no        | ❌ no     |
| `ref`   | heap (ref-counted) | ❌ no       | ✅ yes    | ✅ yes       | ✅ yes    |
| `box`   | heap (ref-counted) | ✅ yes      | ✅ yes    | ❌ no        | ✅ yes    |

These bindings are created within function bodies using those keywords, or created from function arguments depending on parameter passing convention (which is annotated in function signatures):
- `move` => `var`
- `inout` => `var`*
- `mut` => `mut`
- `let` => `let`
- `ref` => `ref`
- `box` => `box`

\* with the restriction that a value must be valid at function exit

### Introduction to Bindings
You may see code in a function body in Mismo that looks like this:
```mismo
var text = get_string()
text.append("more characters")
let alias_to_text = text
```

There are five bindings in Mismo.

- `var`: 
	- allocated inline (on the stack or directly in a field or container)
	- full ownership: can read, write, move, or destroy the data
	- unique — if any part of a var is borrowed (aliased), the `var` is downgraded to a `mut` for the lifetime of that borrow
	- can be sent to other threads (by copy or move, not by alias)
- `let`:
	- second-class borrow
	- implemented as a pointer
	- immutable, read-only view of data
	- freely aliasable; any part or whole of the data can be borrowed by other `let` bindings
	- Can be passed into functions, but in general cannot be stored in structs, containers, or returned from functions
	- cannot be safely shared with other threads
- `mut`: 
	- a mutable version of `let`; follows the same rules except;
	- can mutate data contents, or completely replace data with new value, just cannot destroy/drop the data (only `var` can do that)
	- in addition to `let` borrows, `mut` borrows are also allowed to the whole or parts of the data
- `ref`: 
	- a first-class version of `let`
	- immutable, shareable, first-class reference to data
	- can be returned from functions and stored in structs/containers
	- implemented as a ref-counted, heap-allocated immutable value
	- once data is bound with `ref`, it can thereafter never be mutated by any part of the program
	- thread-safe; can be shared across multiple threads simultaneously
- `box`:
	- a mutable version of `ref`
	- mutable, shareable, first-class reference to data
	- acts just like a `ref` except the data may be mutated, and it is no longer thread-safe (to prevent data-races)

The primary two ways in which data is bound with these bindings is local variables and function parameters.

### Local Variables

Local variables are always declared with one of these five bindings (usually `var` or `let`) like so:
```mismo
var my_variable = <<some expression>>
let some_alias = my_variable
```

### Parameter Passing Conventions
Each parameter of a function signature must be annotated with both a "parameter passing convention" as well as a type.  Each convention corresponds to a specific binding with which the argument is bound in the body of the function.  In terms of what bindings can be passed into a function, conventions may be polymorphic over one or more bindings.  There are six conventions, four of which have the same name as the four non-`var` bindings, and yield the binding of their name, and there are two conventions which both yield `var` bindings, but with different implications.

The six conventions are as follows, along with their corresponding binding:
- `move` => `var`
- `inout` => `var`*
- `mut` => `mut`
- `let` => `let`
- `ref` => `ref`
- `box` => `box`

\* the `inout` convention yields a `var` binding, but the compiler must guarantee that a valid value exists at that binding at function exit.  This allows the lifetime of the `var` to continue in the caller's context.  Otherwise, it would be considered consumed (as in the `move` convention) and would no longer be usable in the caller's context.

The bindings that may be passed into these conventions are as follows:
- `move` <- `var` (consumed)
- `inout` <- `var` (not consumed)
- `mut` <- `var`, `mut`, `box`
- `let` <- `var`, `mut`, `let`, `box`, `ref`
- `ref` <- `var` (consumed), `ref`
- `box` <- `var` (consumed), `box`

Or in table format:

| Convention | var     | ref   | box   | mut   | let   |
| ---------- | ------- | ----- | ----- | ----- | ----- |
| move       | ✅ yes*  | ❌ no  | ❌ no  | ❌ no  | ❌ no  |
| inout      | ✅ yes** | ❌ no  | ❌ no  | ❌ no  | ❌ no  |
| ref        | ✅ yes*  | ✅ yes | ❌ no  | ❌ no  | ❌ no  |
| box        | ✅ yes*  | ❌ no  | ✅ yes | ❌ no  | ❌ no  |
| mut        | ✅ yes   | ❌ no  | ✅ yes | ✅ yes | ❌ no  |
| let        | ✅ yes   | ✅ yes | ✅ yes | ✅ yes | ✅ yes |
\* `var` will be consumed in caller context
\** `var` in function body must be valid at function end

One way of summarizing the above table is with the following four rules:
- `var` can be passed via any convention (though it may be consumed)
- any binding *other than `var`* can be passed via the convention that shares its name
- any binding can be passed via the `let` convention
- `box` can be passed as `mut`

### Aliasing and Lifetime Rules
Data can be borrowed within the scope of a function body according to the same rules as parameter passing conventions.  For example, a `box` can be aliased as `mut` and `let` like so:
```mismo
box my_data = <<some expression>>
mut part_of_data = my_data.part
let another_part = my_data.another
```

However, when borrowing from a `var`, certain transformations apply.  If any part of data bound with `var` is *moved*, then it must be replaced within the same expression.
`ref gotcha = (my_var.data = new_data)`

If part of a `var` is `borrowed` then the `var` is treated as a `mut` for the lifetime of that borrow (or the union of the lifetimes of all borrowed derived from the `var`).

### Exclusivity
`var`-bound variables are always **unique** and **exclusive**.  If you have a `var` the compiler guarantees that no aliases to that value exist at the same time.  Of course, at other points in the program other names will likely refer to that piece of data because it may move in and out of functions, containers, and variables.  

You can read the value (including to clone it), mutate it, move it (to another variable or into a container), or destroy it.

All the other bindings, including `mut`, are shared, and therefore carry certain restrictions to ensure memory safety.

Variables declared with `let`, on the other hand, can be shared (ie, aliased) without limit.  And parts of values (like struct fields and array elements) can be shared with multiple `let` bindings.  The exception is, of course, if those values are exclusive to a `var` binding, in which case an attempted `let` binding will result in a compiler error.

```mismo
let foo = Person("Ryan", 32)
var bar = Person("Claire", 45)

let baz = foo       -- OK
let baz = foo.name  -- OK
let baz = bar       -- ERROR: bar is a var binding, and therefore cannot be aliased
let baz = bar.name  -- ERROR: not even a part of a var binding can be aliased, 
var baz = bar.name	-- (same error as above)
```

### Mutability
Variables declared with the keyword `let` are immutable in that scope.  Note that this does not mean the value is itself fundamentally immutable for the lifetime of the program (the value may be mutable, for example, in the caller's scope if a `var` binding was passed as `let`) but the Mismo compiler guarantees that for the duration of the current function, the value will not change.

Although the *values* bound to `let` variables are immutable in the current scope, the *name* itself can be re-bound to a different value.  In such a case, no values are mutated, only the names used to refer to them.

Variables declared with `var` are mutable.  Properties of structs and fields of arrays can take on new values, and the value itself can be overwritten with a new value entirely.  (This subtle distinction is significant in [[#Parameter Passing Modes]])	

```mismo
let foo = Person("Ryan", 32)     -- This person is entirely immutable for the duration
								 -- of the scope.  Though the name(s) referring to it
								 -- may change.
var bar = Person("Claire", 45)   -- This person may be mutated and/or consumed
								 -- and/or overwritten.

foo.name = "George"         -- ERROR: foo is immutable
bar.age += 5                -- OK: Claire is now 50 years old

foo = Person("George", 10)  -- OK: the previous person ("Ryan") may or may not still 
							-- be accessible through other bindings, but the name foo 
							-- now refers to a different person.
bar = Person("Georgia", 9)  -- OK: This is not merely reassignment. The previous person
                            -- ("Claire") is dropped, and a new person ("Georgia") is 
                            -- now occupying the `bar` variable.
-- Note: If `bar` was passed in via the `mut` argument
-- passing mode, then that value has now been replaced in
-- the caller's scope as well.

```


### Constants
I think I may also allow local constants like `const foo = Person("Bob", 22)` which behaves just like a let, except that it cannot be re-bound to another value.  The value is calculated at runtime.

Then there are also compile-time constants that are defined at the top level with the same syntax.

### Assignment
Dimensions of assignment:
- destination (LHS) convention: let, mut, sink, copy, none (var defaults to sink)
- source (RHS) type default convention (usually let or copy)
- source access: read, write, move
- passing mode override

```mismo
let person_one = Person("one") <-|
^                                |
source_access: read              |
var foo = copy person_one        |
^         ^                      |
|         override               |
|                       type_default: let
dest_convention: sink

[let|var] <name> = [let|mut|sink|copy]? <expr:access:type>
```

I think I am realizing that the whole idea of "default passing convention" for a type may be slightly misleading.  Rather, I just a boolean: `implicit_copy_allowed?`.  Such types should always get passed by copy *unless* a `mut` is specifically requested
## Functions

Every program in Mismo launches from the function called `main`.  If the `main` function is missing, the code can be imported, but it cannot be run directly.

```
fn main
	print("Hello world!")

fn add(a Int, b Int) Int
	a + b
```

### Argument Passing Modes

              param 
           | let | var  | copy | move
    ———————+—————————————————————————
      let  | let | err  | copy | err
arg   var  | let | var  | copy | err
      copy | let | copy | copy | copy
      move | let | move | copy | move

|         |          |         | **PARAM** |          |          |
| ------- | -------- | ------- | --------- | -------- | -------- |
|         |          | **let** | **var**   | **copy** | **move** |
|         | **let**  | let     | --        | copy     | --       |
| **ARG** | **var**  | let     | var       | copy     | --       |
|         | **copy** | let     | copy      | copy     | copy     |
|         | **move** | move    | move      | move     | move     |
	
```
if arg == move:
	move
elif param == let:
	let
elif arg == copy or param == copy:
	copy
elif param == var == var:
	var
else:
	raise error
```

### Function Overloading
In lieue of class methods, Mismo allows arbitrary function overloading.  At the call site, the Mismo compiler will attempt to match the arguments to the overload based on the types of the arguments as well as the argument passing modes of the arguments.  

Sometimes, there may be multiple possible overloads that match the call if, for example, there are two generic overloads that.  The compiler complains when it encounters such a case, and it will suggest three ways to disambiguate the call:
- explicitly specify the type parameters (if one or more overloads is generic)
- explicitly specify the argument passing modes
- explicitly specify the names of the parameters

Here are some examples where each of these might be useful:
#### Disambiguation by Type Arguments
```mismo
-- declarations
fn print[T Stringable](arg T)
	...
fn print(arg String)
	...

-- call site
print("hello world")
-- ERROR: ambiguous function call `print("hello world")`
-- try specifying type parameters to disambiguate

-- specify the first overload like this:
print[String]("hello world")

-- specify the second overload like this:
print[]("hello world") 
```

#### Disambiguation by Argument Passing Modes
```mismo
fn sort[T](mut array Array[T])
	...

fn sort[T](ref array Array[T])
	...

-- ambiguous function call
var my_list = [2, 2, 5, 3]
my_list.sort


-- solution: specify argument passing mode
sort(mut my_list)
-- or
sort(ref my_list)
```

#### Disambiguation by naming parameters
```mismo
fn move_thing(mut thing Thing, left Float)
	...
fn move_thing(mut thing Thing, right Float)
	...

-- ambiguous function call
var thing = Thing
thing.move_thing(30.72)

-- solution: specify parameter names:
thing.move_thing(left: 30.72)
-- or
thing.move_thing(right: 30.72)
```

#### Suggestions
In general, the programmer should try to write overloads that the compiler can disambiguate without the need for these strategies.  This can be done by:
- simply limiting the number of overloads to only one or two
- overload based only the type of the first argument
- writing one generic function rather than several overloads

Function overloading is meant to allow grouping of functions that have similar *semantics* but different *implementation*.  If two overloads have a similar implementation, they should probably be merged into one function.

But in the end, it is up to the programmer to ensure a friendly API, whatever it may look like.

#### Idea: Function Overload Syntax
To define a top-level function with overloads:
```
function move_thing
	fn (mut thing Thing, left Float):
		...

	fn (mut thing Thing, right Float):
		...
```

To define a single top-level function (not overloaded):
```
function fibonnaci(n Int) Int:
	...
```

To add additional overloads to an already declared function:
```
extend fibonacci(n Float) Float:
	...
```

Using the `function` declaration word for the same function name twice results in a compiler error.  The `impl` keyword is needed in this case.  This is useful, for example, when adding overloads to a function declared in another module.

The `impl` keyword can also be used for types (structs and enums... probably not traits) to add more methods.

```
struct Location
	let col Int
	let line Int

	fn +(offset Int) Location:
		Location(.col + offset, .line)

extend Location
	fn mut +(offset Int):
		.col += offset
```

#### When disambiguation is not enough...
I just had a lengthy conversation with Gemini, Perplexity, and Claude about the limitations of this strategy for disambiguation.  This code exemplifies the difficulty:
```
function bar 
	fn [T](a T, b Int): ...    -- overload A    
	fn [U](a Int, b U): ...  -- overload B

function foo[S Stringable](arg S): 
	bar(arg, 5) 
	bar[Int](arg, 5)
```

First, we must recognize that we **should not** pick a specific overload for a function call that includes non-concrete types, such as `bar(arg, 5)` since `arg` is of abstract type `S`.   We must wait for `foo` to be instantiated with a concrete type before we can even check if the `bar` call is ambiguous or not.

Secondly, *whether or not bar is ambiguous depends on the concrete type supplied to `S`*.  This may apply even when type parameters are specified concretely, as in the second call to `bar[Int](arg, 5)`.  If `S` is concretized as `Int`, then both function overloads still match.

So I'm leaning towards the option of **banning overlapping generic function definitions**.

#### Banning Overlapping Function Overload Definitions
The primary downfall of this approach is the loss of the "specialization pattern", eg
```
function average 
	fn [T](numbers Array[T]) T: ... -- Overload A: Generic average 
	fn (numbers Array[T]) Float: ... -- Overload B: Specialized average for Ints
```

There are certainly workarounds, but still, presents some friction.

There is also the challenge of implementation: How can you tell when function overloads are potentially ambiguous?

The strategy is, to check for potential ambiguity between two functions, go through the parameters one by one, and test to see if there is possible overlap between the types.  If the number of parameters is the same between the two functions, and all parameters have overlap, the function signatures overlap.

How do test if parameter types do or don't overlap?
- If both types are concrete types, then they overlap only if they are the same type
- If one is concrete and the the other is a trait bound, then they are considering non-overlapping if trying to implement the trait bound for the concrete type would lead to a compile error
- If both are trait bounds, then they overlap if  it's possible for one type to implement both

How do we test if a type can implement a trait bound or not?
A type cannot implement a trait only if it implements at least one method with a signature that is incompatible with a required method signature of the trait.

What are incompatible method signatures?
Incompatible method signatures are function overloads with potential overlap/ambiguity at the call site.

Oops, now our definition of non-overlapping function overloads is circular.  Could this lead to paradoxes and/or undecidable cases?

```
function foo
	fn [T Stringable](t T)
	fn (U Countable)(u U)

-- compare t T and u U
-- => Stringable & Countable?

trait Stringable
	fn string String

trait Countable
	fn count Int

-- in this case, Stringable and Countable are compatible, since they don't have any overlapping method names... but what about Stringable and Format?

function foo
	fn [T Stringable](t T)
	fn (U Format)(u U)

trait Stringable
	fn .string String
	fn .foo[T Format](t T)

trait Format
	fn .string String
	fn .foo[U Stringable](u U)

struct Item is Stringable & Format
	fn .string String:
		"item"

	fn .foo[T Format](t T): ...
	fn .foo[U Stringable](u U): ...

-- in this case, we have two methods with the same name, so let's test if the overloads are compatible or not:

-- compare parameters self Self, and self Self
```

##### Solutions?
We could be conservative and forego comparing trait bounds an just assume that all traits are compatible for the purposes of overlap detection.  In this case, **negative trait bounds** might become useful.
```
function foo
	fn [T Stringable & not Format](t T)
	fn [U Format & not Stringable](u U)

	-- now these signatures are provably disjoint
```

Alternatively, we could write an algorithm that detects circular dependencies and then either issue a warning or an error in only those cases ... I think this would be rare in practice?  But implementation is more work.  Plus, this could be added on to the first strategy afterward if I change my mind.

Additionally, even if the compiler is able to do it, this may make it slightly easier for the writer of the code, but may obscure the meaning for the reader of the code, especially if it is not obvious which traits are compatible or not at a glance.

### Multiple Dispatch
Mismo does not support dynamic dispatch.  Instead, polymorphism is achieved with enums, and generic functions with trait constraints.

## Control Flow
### Ultimate Conditional Syntax
```grammer
if <test> : ...

if <test> :
    <stmt>
else: 
    <stmt>

if 
	<test> : ...
	<test> : ...
	else: ...

if <expr>
    <op> <expr> and <anything_that_comes_after_if>
    <op> <expr> : ...

if <expr> <op>
    <expr> and <anything_that_comes_after_if>
    <expr> : <stmt>
    _: <stmt>


<match_expression> ::= if <branch>+
<branch> ::= <expr> (<end> | <op_split>+)
<op_split_> ::= <op> <branch>+

term ::= <test> : <stmt>
      |  <expr> <begin_block> (<op_split>+ | <final>)
      |  <expr> <op> <begin_block> <term>+
op_split ::= <op> (<term split>+ | <final>)
final ::= ":" <stmt>
```

```samples
if x == 1: ...

if y is 2: ...

if
	y 
		==
			1
			2
			3
		<= 
			1
			2
			3
	x > 0: ...
	x >
		0
		1
		2
	x
		> 0
		> 1
		> 2
	else: ...

if x >
	100: ...
	10: ...
	1: ...
	else: ...

if x >
	100: ...
	10: ...
	1: ...

if x
	< 0: ...
	== 0: ...
	> 0: ...
	
if x is 
	y: ...
	0: ...
	_: ...
```

```
fn sign(x Int)
	if x
		< 0: -1
		> 0: 1
		else: 0

sign(-34)  -- -1
sign(12)   -- 1
sign(0)    -- 0

fn f(x Int) String
	if x
		== 0: "null"      -- split before operator
		< 0: "negative"
		% 2 == 
			0: "even"     -- split after operator
			_: "odd"      -- _ is a non-binding wildcard pattern

f(0)     -- null
f(-100)  -- negative
f(2)     -- even
f(4)     -- even
f(7)     -- odd

fn sum(tup (Int?, Int?)) Int
	if tup is
		(Some(a), Some(b)): a + b
		(Some(a), None) or (None, Some(a)): a
		else: 0

fn fib(nth Int) Int
	if n ==
		0: 1
		1: 1
		else: fib(nth - 1) + fib(nth - 2)

fn fizzbuzz
	for n in range(0, 30)
		if (x => n % x == 0)
			3 and
				n /? 5: print("fizzbuzz")
				else: print("fizz")
			5: print("buzz")
			else: print(n)

fn main
	arr Array[Int]
	arr = make_some_array()
	if arr[2] is
		Some(i) -> print(i)
		None -> print("arr is less than three elements long")

	mixed_array Array[Int | Float] 
	mixed_array = [1, 1.5, 2, 2.5, 3]
	if arr[2] is
		Some(n) and n is Int

```


```MLScript
fun fizzbuzz(n) = if n % 
	3 == 0 and 
		n % 5 == 0 then "fizzbuzz"
		else then "fizz"
	5 == 0 then "buzz"
	else then n

fizzbuzz(0)
fizzbuzz(1)
fizzbuzz(2)
fizzbuzz(3)
fizzbuzz(5)
fizzbuzz(15)
```

### Loops
Mismo has only one loop construct: `while`.

An infinite loop is written like this:
```
while true:
	print("this song goes on forever")
```

A C-style for loop is written like this:
```
var i = 0
while i < 10:
	print(i)
	i += 1
```

An iterator-based loop looks like this:
```
var iter = make_iterator()
while iter.next is Some(item):
	print(item)
```

Mismo does supply syntax sugar for this common case:
```
for item in iterable:
	print(item)
	
-- desugars too:
var _temp_iter = (iterable).iter
while _temp_iter.next is Some(item):
	print(item)
	
	
-- likewise the expression
for person in people:
	if person.name = "Steve":
		break Option.Some(person)
else:
	Option.None
	
-- desuagars too
var people_iter = people.iter
while people_iter.next is Some(person):
	if person.name == "Steve":
		break Option.Some(person)
else:
	Option.None

```

## Memory Safety
Mismo is a memory-safe and thread-safe language whose safety is built on affine type and mutable value semantics.

Originally, I wanted to just have two *bindings* (where a binding is more-or-less equivalent to Pony's reference capability): `var` and `let`, where the first is mutable and exclusive and thread-safe, and the second is immutable and aliasable — and cannot be stored in structs or closures.  Then there were also a few parameter passing conventions: `let` would produce a `let` binding, and the rest would produce a `var` binding with different trade-offs:
- `sink` => end the lifetime in the caller
- `copy` => split into two lifetimes; caller and callee separate
- `mut` => lifetime continues in callee, but contract that the parameter cannot be in a "consumed" state when function returns

This system is indeed memory safe, but very restrictive.
### Problem: too restrictive
- first problem, which is easy to solve, is that of thread-safe immutable values (Pony's `val`); sometimes we want to share values across threads, but we don't need to mutate those values, then we should have a safe way to do that without cloning: so we introduce the `ref` binding which is like Rust's `Arc` (atomic reference counting) to share immutable values freely across threads.
- second problem: sometimes shared mutability is really useful (eg for graph-like structures) so we introduce the `box` binding (like Rust's `Rc`) which is heap-allocated, reference counted, and not thread-safe, but can be stored in structs and allows mutability from multiple perspectives.

So now we have four bindings:
- `var` => exclusive, sendable, mutable
- `let` => aliasable, not-sendable, immutable
- `box` => aliasable, not-sendable, mutable
- `ref` => aliasable, sendable, immutable

And at this point I thought it was pretty complete.  But then I realized there are still some basic things that you can't do with a `var` that would force programmers to fall back to `box`.  This is bad because my vision is for `var` and `let` to really be very usable everything a programmer wants to do, and minimizing the situations where `box` and `ref` feel needed.

### The real problem: shared mutability ⇒ dangling references or UB
Here's a situation to illustrate the issue
```mismo
fn main:
	var my_tuple = {3, 4}
	mut first_elem = my_tuple.0
	my_tuple.set_second_element(9)
	print(first_elem)
```

In the system as-is, this would be a compile error, because the `mut first_elem` is still alive when `my_tuple` is mutated — so it fails to get the exclusive mutable reference it needs. That's really unfortunate, because this program is actually totally memory-safe.  

So why not just make `mut` aliasable?  Well, one reason is that precludes thread-safety.  But there's also another reason that doesn't even have anything to do with concurrency.  Here's another example that illustrates that:
```mismo
fn main:
	var people = [Person("Alan"), Person("Beth"), Person("Carl")]
	var another_person = Person("Daphne")
	mut last_person = people.mut(people.count - 1)  -- Carl
	if last_person.name < another_person.name:
		people.push(another_person)

	print(last_person.name)
```

Again, we are attempting to obtain multiple mutable references to a single value.  But in this case, we can see the value of the restriction: naively allowing aliasable `mut`s may lead to memory unsafety because a modification to `people` may *invalidate* the mutable borrow to `last_person`.  If, for example,  the call to `push` reallocates the underlying buffer, then `last_person` is now a dangling pointer.

Of course, there are ways of working around this.  This example could easily be refactored to use closures or printing the name earlier.  A more complicated example might be really annoying to refactor, and the developer would like resort to either cloning or `box`.  

Plus, this problem is not just with dynamic arrays.  Other data structures that (re)allocate have this problem, like maps and linked lists, as well as data structures that "change shape" like enums with data.

So is there a better way?

#### Solution One: panicky references
One possibility is to allow second-class borrows in the same way, but implement it in such a way that the runtime can detect when the structure to which the borrow belongs has changed shape, and panic when it is accessed after such a shape-change.

For arrays, a mutable reference would then need to be implemented as a pointer+offset struct; a pointer to the dynamic array containing the buffer, and an offset into that buffer.  Then any dereference of the `mut` (or `let`) reference would have to do bounds-checking and panic in the case the index is outside the buffer range.

For enums, the reference would have to store a pointer to the discriminant, the expected discriminant, and also either an offset or direct pointer to the value, and every dereference would panic unless the discriminant matches.  (A smarter implementation might also check overlap of the referenced field between variants.)

I believe this works, but there are at least three downsides:
- minor performance hit for an additional check on every dereference
- disconcerting possibility of a panic at runtime that may not be caught at compile-time (difficult to debug?)
- may lead to unexpected results if an array is sorted, or an "object" moves within an array away from the index we are expecting to find it at

Due to these problems, it's fair to ask if this is really much better than a raw pointer with undefined behaviour.

#### Solution Two: `mut` = shape-stable borrow and `inout` = owned borrow
Alternatively, we could add yet another binding specifically for shape-stable mutations.  We should probably call the binding `mut` and it would be freely aliasable, but not sendable, and also restricted to mutations that are not shape-changing, ie, re-assigning an enum to a variant with different fields, or popping/pushing an array.

Now we have a total of **five** bindings:
- `var` => exclusive, sendable, mutable
- `mut` => aliasable, not-sendable, mutable (only shape-stable mutations)
- `let` => aliasable, not-sendable, immutable
- `box` => aliasable, not-sendable, mutable
- `ref` => aliasable, sendable, immutable

Then the `mut` parameter passing convention would yield a `mut` instead of a `var`, and a new convention, `inout` would take the place of our previous `mut` convention, yielding an exclusive `var` but still applying the restriction that it must be valid when the function returns.

I need to think about this more.  This definitely complicates our model a bit more.  One may wonder, for example, what the difference is between `box` and `mut`.  But at least it offers more safety and consistency.

### Viewpoint Adaptation
Example from Pony: https://tutorial.ponylang.io/reference-capabilities/combining-capabilities


| Origin | var field | box field | ref field |
| ------ | --------- | --------- | --------- |
| var    | inout     | box       | ref       |
| mut    | mut       | box       | ref       |
| let    | let       | let       | ref       |
| box    | inout     | box       | ref       |
| ref    | let       | let       | ref       |

### Binding Polymorphism

If methods like `iter` are polymorphic (overloaded) over bindings, I need some good syntax for "downgrading" the binding.  For example, if I have a `var` array, but I don't want to 
```mismo
fn main:
	var people = [Person("Alan"), Person("Beth")]
	mut peeps = (people as mut).iter   -- a little verbose
	mut peeps = mut people.iter        -- ambiguous: is it `(mut p).iter` or `mut (p.iter)`?
	mut peeps = (mut people).iter      -- annoying extra parens
	mut peeps = iter(mut people)       -- shame you can't use UFCS
	mut peeps = people.iter_mut        -- doesn't necessarily extend to arbitrary methods
	mut peeps = people.mut.iter        -- restricts user-defined overloads of `mut`
	mut peeps = people.as_mut.iter     -- super clear, ergonomic, only slightly longer
									   -- just needs slight adjustment for `box` and `ref`:
									   -- `people.to_box` or `people.into_ref`
```


## Types
### Structs
#### Fields
Structs are declared with the `struct` key word, and contain fields declared with either `var` for mutable fields, or `let` for immutable fields.
```mismo
struct Person
	var name String
	var age Int
```

Fields declared with `let`are **deeply immutable**, meaning they cannot be swapped out after construction, and any sub-fields are also immutable.

#### Struct Embedding
Structs can also **embed** other types.  Embedding is done with the `embed` keyword where a field would be, followed by a type.  When struct `Bar` is embedded into struct `Foo`, then all instances of `Foo` will have all the fields of `Bar`.  Furthermore, `Foo` also has a field called `Bar` that refers to all those fields in aggregate.  
```mismo
struct Employee
	embed Person
	let role String

fn main
	let _joe = Person("Joseph", 37)
	let joe = Employee(sink _joe, "Custodian")
	print(joe.name)  -- "Joseph"
	print(joe.role)  -- "Custodian"
	print(joe.Person)  -- Person("Jospeh", 37)
```

This is one way to do a kind of inheritance.  It can help to reduce typing of many fields repeatedly, and it means any instance of `Employee` can be implicitly passed to any function expecting a `Person`.

#### Alternate Struct Syntax
Although the above syntax is the suggested syntax for structs, some simple structs may also be declared using a more concise inline syntax.  All fields are put in parentheses separated by commas, and the "let"/"var"/"embed" command is dropped.
```mismo
struct Employee(Person, role String)
```

It is not possible declare `var` fields and methods when using this syntax.

### Methods
As Mismo features unified function call syntax, methods in Mismo are actually just regular functions, but with some syntax-sugar.  Any function declared indented under a type gains an implicit first parameter called "self".  Within the function body, fields of self may be accessed like `self.field` or simply `.field`.  The parameter passing convention is specified between the `fn` keyword and the name of the function, or defaults to `let` if missing.
```mismo
struct Person
	let name String
	var age Int
	
	fn mut increment_age(years Int = 1):
		.age = .age + years

-- the above method is equivalent to this function:
fn increment_age(mut person Person, years Int = 1):
	person.age = person.age + years
```

#### Fields are also methods
Declaring a field in a struct automatically defines a method with the same name as the field that retrieves the value of that field.  If it is a `var` field, then a second method is also automatically defined.  If defined manually, the struct declaration would look like this:
```mismo
struct Person
	let name String
	var age Int
	
	fn name String
		-- return the value of the "name" field of self
		
	fn age Int
		-- return the value of the "age" field of self
		
	fn mut age=(value Int) Int
		-- set the "age" field of self, and return the old value
```

By the way, any method with a name ending in the equals sign that also includes a parameter called "value" can be called implicitly with the assignment operator.  If there are additional parameters, they will appear on the left-hand-side of the equal sign.  Example:
```
struct MyDict
	fn mut dict=(key String, value Int) ...

fn main
	var d = MyDict
	d.dict("key") = 5
	-- desugars to:
	d.dict=("key", 5)
```

Traits can also require methods of this form.  This is a way in which a trait can encourage the existence of certain fields in types that implement it, and their mutability.  We say "encourage" rather than "enforce" because a type could of course define these methods without any underlying data.

### Enums
Mismo provides flexible enums as a way to do union types.

```mismo
enum Color
	Red
	Blue
	Green
```

Enum variants are also structs, and therefore can contain fields, and even embed other enums and structs.  The syntax is more terse for enum variants; the following two are equivalent:
```mismo
enum Token
	Dot
	Identifier
		let name String
	IntLiteral
		let value Int
```
can also be written as:
```mismo
enum Token
	Dot
	Identifier(​name String)
	IntLiteral(value Int)
```

However, in the above case, struct embedding might be convenient:
```mismo
enum Token
	Dot
	Identifier
		embed String
	IntLiteral
		embed Int
```
can also be written as:
```mismo
enum Token
	Dot
	Identifier(String)
	IntLiteral(Int)
```

The more concise syntax is to be preferred in all cases except if a particular variant requires 'var' fields or more complex initialization logic.

Enum instances are instantiated as follows:
```mismo
let tok = Token.Dot
let tok = Token.Identifier("x")
let tok = Token.IntLiteral(820)
```

In match statements, the "Token" prefix may be omitted.
```mismo
let tok Token = get_token()
if tok is
	Dot: -- do something 
	Identifier(name): -- handle `name`
	IntLiteral(i): -- use integer i
```

The prefix can also be omitted in cases where a variant consists of a single embedded type whose name is the same as the variant.  
```mismo
struct Review
	let reviewer_name String
	let rating Int
	embed String

enum Eval
	Negative(reason: String)
	Neutral
	Positive(reason: String)
	Review(Review)

fn main
	var evals Array[Eval] = []
	evals.push(Eval.Negative("didn't like it"))
	evals.push(Eval.Neutral)
	evals.push(
		Review("Kristof", 5, "...")  -- here we can omit the Eval.Review prefix
	)                                -- the value of type Review is automatically coerced
									 -- to type Eval.Review

	for eval in evals:
		if eval is 
			Negative(r): print("bad review: " r)
			Neutral: print("okay review")
			Positive(r): print("good review: " r)
			Review(r): print("detailed review by " r.reviewer_name ": " r)
	
```

### Singletons
In Mismo, an empty struct can be treated as a singleton.
```mismo
struct None

fn main
	let none = None
	let other_none = None
	none == other_none --> true
```

### Collections

#### Example of Singly-Linked List Implementation
```
struct List[T]
	head Node[T]?

	fn get(idx Int) -> let T?
		node : .head      -- bind the name node to self.head
							  -- compiler infers lifetime of node to be that of self
		while node and idx > 0
			node : node.next  -- compiler infers lifetime of node to be self
			idx -= 1		  -- because next => node => head => self
		node                  -- OK: compiler infers lifetime of node to be self|static
							  -- (might be static because lifetime of None is static)

struct Node[T]
	slot data T
	slot next Node[T]?

struct List[T]
	head Node[T]?

	fn get(idx Int) let T?
		let node = .head
		while node and idx > 0:
			node = node.next
			idx -= 1
		node
			
```

### Closures
There are two concepts in Mismo that serve the role of closures: subroutines and callable singletons.

A **subroutine** is a value-type in Mismo and created with the keyword `sub`.  Subroutines capture variable by reference (let or var) only.  Subroutines themselves can never be bound to a `val` variable, and therefore cannot be stored in arrays or structs, and cannot be returned by functions.  Subroutines can, however, be passed as arguments to functions.

```
fn main
	var ryan = Person("Ryan", 32)
	var change_age = sub (by Int) {
		ryan.age += by
	}
	if today == ryan.birthday: change_age(1)
	print(ryan.age)  -- 33

fn main
	val ls = [1, 2, 3, 4]
	ls.map(sub (n Int) Int {n * n})
	print(ls)  -- [1, 4, 9, 16]
```

#### Callable Singletons
Singleton objects are created with curly braces.  Singletons are their own type.  Singletons have properties that 

```
fn main
	var iter = make_iterator([1, 2, 3, 4])
	print(iter())  -- 1
	print(iter())  -- 2
	print(iter.i)  -- 2

fn make_iterator[T](val ls Array[T]) {fn Option[ref T]}
	val i = 0
	return {
		val i    -- explicitly capture variables by value
		val ls
		fn T     -- nameless method; this is what is called when using the singleton as a function
			if i == ls.count: return None
			i += 1
			ls.get(i-1)
	}
```

### More Types

### Nominal Types and Structural Types
```
-- product types
struct Point
	var x: Int
	var y: Int
	
-- structural equivalent: (Int, Int)

-- sum types
enum Error
	Code(Int)
	Message(String)
	Full(Int, String)
	
-- structural equivalent: (Int | String | (Int, String))

fn main: 
	let err: (Int | String | (Int, String)) = 404
	if err is
		(n: Int) =>  -- do something with n
		(s: String) => print(s)
		(n: Int, s: String) => format_err(n, s)
```

## Traits
Traits are not types, per se, but types of types.  Interfaces are used as constraints for type parameters of generic functions and generic structs.  Interfaces consist of a list of function signatures.  A given type is said to satisfy an interface if it implements all of the required functions.

```mismo
trait Stringable
	fn string String
```

Any struct or enum that implements a `string` function satisfies the `Stringable` interface.  Interface implementation does not need to be declared as such, it can be implicit and the compiler will detect it.

An interface is then used like this
```mismo
fn print[T Stringable](arg T)
	let s = arg.string
	...

"string interpolation: \ expr + 1 \ times cool"
```

An interface is not a type and you cannot declare a variable typed as an interface.  The following code will not compile:
```mismo
struct Essay
	let author: String
	let content: String
	fn string String
		.content

fn main
	let text Stringable      -- ERROR: Stringable is an interface, not a type
	if some_condition:
		text = "Some string literal"
	else:
		text = Essay("Briones", "this is a great essay")
	print(text)
```

You would need to use enums to achieve an effect like this:
```mismo
enum WrittenWork
	String(String)
	Essay(Essay)

fn main
	let text WrittenWork
	if some_condition:
		text = "Some string literal"
	else:
		text = Essay("Briones", "this is a great essay")
	print(text)
```

Interfaces can also provide default function implementations.  However, in order to take advantage of them, a type must declare itself as an interface.  (The standard library has many interfaces with default functions implementations that you can take advantage of.)

```mismo
interface Sized
	fn size UInt
		0
```

### Dynamic Types & Function Members
How does polymorphism work in Mismo?  

Well, function overloads and generic functions allow one function name to dispatch on multiple different types.  However, overload resolution and generic function instantiation are all resolved at compile time.  

So how can we achieve runtime polymorphism?

There are two ways:
#### Runtime Polymorphism via Sum Types
Basic polymorphism over a closed set of variants can and should be done with Mismo `enum` types.  

```mismo
enum Token
	Dot
	Identifier(String)
	Number(Float)
	
	def string String:
		if self is
			Dot: "."
			Identifier(s): s
			Number(f): f.string
```
#### Runtime Polymorphism via Function Members
However, if you want a type that can vary its behaviour at runtime and all the variants are not necessarily known to the developer of the type, then the solution is a struct of function members.

```mismo
struct Writer
	var write Function[String]
	
	def write_line(str String):
		.write(str)
		.write('\n')
		
def ConsoleWriter Writer:
	Writer(|str String|: STDIN.write(str))
	
def FileWriter(path String) Writer:
	var file = File.new(path).or(panic)
	Writer(|str String|: file.write(str))
		
def main:
	var console = ConsoleWriter()
	var filer = FileWriter("log.txt")
	var writers Array[Writer] = [console, filer]  -- since the writers are both of the same 
												  -- type, they can be stored in one array 
	for w in writers:
		w.write("test")  -- thus dynamic dispatch is achieved
	
```

When the compiler encounters the syntax `w.write("test")` it is parsed as a function call `write(w, "test")`.  However, since the compiler sees that `w` is of type `Writer` and `Writer` has a callable member called `write`, that member function is added to the overloads considered by the compiler .  In other words, it's as if the compiler has generated an overload of `write` that looks like this:
```
def write(self Writer, str String):
	let fn = self.get_property("write")
	fn.call(str)
```

## Syntax
### Whitespace
Mismo is mostly whitespace sensitive within function bodies, but not whitespace sensitive in function declaration headers, or in type declarations.

Within function bodies, generally speaking, `\n` is statement separator so each line is its own statement, unless the following line is indented.

More specifically:
- `\n` or `;` or `;\n` is a statement separator
	- **except** if the following line is indented one or more beyond the current indentation (then it's considered an extension of the same expression)
		- **except** if the indented line follows a colon (then it's considered an indented block)

For example, all of these are considered one statement:
```mismo
struct VeryLongTypeName[T SomeTrait]          
		is VeryLongTraitName & AnotherTrait  -- this is considered one line 
	let field String
	...

fn main
	"some" + "expression"     -- these two lines are separate statements
	"another" + "expression"  -- starting this with an operator would be an error
	
	1 + 2 + 3
		+ 4 + 5 + 6   -- indented line considered extension of statement above
		+ 7 + 8 + 9   -- you can have as many additional lines as you want
	
	if condition == true:
		some_expression(3)  -- this is considered a block, because the 
							-- indentation follows a colon

	long + expression + that + includes + (a - parenthesized 
		- expression - inside - as well
		- that - might - even - take - three - lines )
	long + expression + (
		"this is a block";
		"return this last value" + if some_value is
		asdfasdf)
```

#### Questions / Awkardness
1. if an `if` statement appears in a line-continuation, the following "indent" may visually line up with the if statement (not be indented)
2. all finishing parentheses and brackets must also be indented
	- if you can't have something like this:
	```
	function_call(
		arg1,
		arg2
	)  -- this is an awkward new statetment 
	```
3. 

#### Parsing UCS
Note the following two syntaxes:
```mismo
-- Sample 1
if 
	x
		==
			1: "one"
			2: "two"
			3: "three"
		> 
			4: "gt four"
			5: "gt five"
			6: "gt six"
	y == 3: "y is 3"

-- Sample 2
if x ==	1: "one"
		2: "two"
		3: "three"
	<=  4: "gt four"
		5: "gt five"
		6: "gt six"
	y == 3: "y is 3"
```

- In Sample 2, how do we know that the `2: `  is a right-term of ` ==` and not a whole test?
	- that sounds like a parsing challenge not worth the effort — it would probably end up being humanly ambiguous anyway
	- therefore, distinguishing between ucs tests and right-terms should be done with indentation (as in Sample 1)

So, let's revamp that whitespace blurb:
### Whitespace
Mismo is *mostly* whitespace insensitive.  The compiler can tell when an expression is finished or not, so feel free to write expressions spanning multiple lines.  But if you want multiple expressions on one line, then they must be separated by a semi-colon.  This is enforced for readability's sake, and to prevent bugs.

The only restriction for an expression spanning multiple lines is that the continuing line **cannot** begin with a an **opening parenthesis** `(` or **opening bracket** `[`.  When at the beginning of a line, those characters will not be interpreted as function call or arguments to a generic, but rather as a regular grouping or array literal.

There are also a few contexts in which indentation is significant to the compiler:
- a function declaration indented under a type declaration is interpreted as a method of said type
- an indented line following a **colon** indicates the start of a new block
- an indented line in a conditional expression is used to branch tests. (see [[#Ultimate Conditional Syntax]])
- any line whose indentation is less than that of the current block indicates one or more block endings
	- specifically, a line whose indentation matches the indentation of a block above indicates the closure of all blocks indented deeper

### Whitespace v3
Observe the following examples:
```
struct Type
	   fn mut long_function_name(
	mut param1 Type,
	param2 Type
	) 
	ReturnType
	:
	  this is the_body

	fn mut(
		param1
	) ReturnType: 
		this is the_body

	fn foo
 :
  func_body

	fn foo(
		arg):
	  function_body
```

Now these are examples you wouldn't expect to see in the wild, but if you feed them to the compiler what should it do?

```
struct Type[
	T Stringable
	U Indexable
] 
is 
Trait
let n Int
var name String


```

#### Declaration Divisions
The lexer spawns a parser precisely when:
- in_function_body == false
- last_token is one of: struct, function, enum

This allows type declarations to span multiple lines at the first level without the lexer dissecting them.  And field and method definitions can also be non-indented, if the programmer so chooses.

#### Method Divisions
A method division is detected very simply by a `fn` token outside a function body.  How do we know when we have exited a function body?

Two types of function bodies: 
- single-line function
- indented block

When the lexer encounters a colon:
- flag `single_line_function`
- if the next token is a `BeginBlock`, then unflag it
- otherwise, unflag it after the first newline, no exceptions, at which point `in_function_body` also gets unflagged
- But if a block is encountered, the `in_function_body` flag remains until `block_indents.size() == 0`

A `BeginBlock` is emitted when indentation is detected following a colon.  Indentation is detected by 

```
function main:
	for i in 
	array:
		something

		 for i in
	   array:
	    print(1 + i)
	   for i in
	  array:
	   print(1 + i)
	if x >
		y + very + long
		+ expression: consequent
		z: consequent
		
```

### Whitespace v4 (inspired by Ante)
#### Inside Function Blocks
- an expression may span multiple lines, but all tokens must be indented deeper than the first token
- EXCEPT: an `else` token or closing bracket (`]`, `)`, `}`) may occupy the same column as the first token
- opening paren and opening brackets that would otherwise be interpreted as a function call or type args is actually considered a syntax error if it 

```
function main
	this + isnt + a + function_call
		(arg1, arg2)
	but + this + is
		+ a + function_call(
		arg1, arg2
	)
```

#### In array literals and argument lists
Possibilities for separators:
- comma
- comma + newline(not followed by ender or unindent)
- newline(not block-end)
- newline(not block-end) + comma
- newline(not block-end) + comma + newline(not followed by ender or unindent)
- 

### Commas, Semicolons, Tuples
I had an idea that might make parsing a little bit simpler.  The idea itself is simple too: commas are only valid in a list context (ie, argument/parameter list, tuple/array literal).  In other words, creating tuples out of a kind of comma operator is not allowed.

What's the benefit?  Two things:
1. it allows us to use regular parentheses for blocks, if so desired for scoping, etc
```mismo
fn main:
	let x = (
		var y = foo.calculate(1, 2)
		y += y.bar
		y
	)
```

Wait a second, if I use `{}` for tuples, then that above example works anyway.  If I switched it around, using parens for tuple literals, then I would have to use `{}` for blocks, because for consistency we would want to parse the above as a 3-tuple (as if each newline was a comma).

2. it allows a closures body to be terminated by a comma in an arg list

```mismo
fn main:
	collection.map(\x => 
		var y = some_function(x, 2)
		x + y > 8,
		some_other_arg
	)
```

Okay, admittedly that comma separating the two arguments is not really clear.

```mismo
fn main:
	collection.map(
		\x => 
			var y = some_function(x, 2)
			x + y > 8
		some_other_arg
	)
```

```mismo
fn main:
	collection.map((\x => var y = some_function(x, 2); x + y > 8), some_other_arg)
```

Those latter two are both probably much more clear... so (2) isn't a real advantage.


### Generic Type Parameters & Constraints
I'm pivoting towards having interfaces semantically always having at least one type parameter, the first of which is always called `Self`.  This simplifies a few things implementation-wise, but it is asking for more syntax sugar.

For example, I don't want to, when defining a function signature, declare a type variable three times:
```
def print[T](msg: T) with Stringable[T]: ...
```

It should be at most twice, maybe even once if possible.  So I thought of the following simpler syntax:
```
def print(msg T#Stringable): ...
```

There is no explicit list of type parameters, rather, they are introduced anywhere in the definition or body using the `#` symbol.  `#` says "what precedes is a generic type parameter, what follows is a constraint".  This also very naturally allows for type names without constraints (eg `T#`) as well as anonymous type parameters, with or without constraints.
```
def print(msg #Stringable): 
	OS.StdOut.println(msg)

enum Option
	Some(#)
	None

enum Result
	Ok(T#)
	Error(R#)  -- this

trait Equatable
	def ==(self: Self, other: T#Equatable[Self])

struct ComplexDataProcessor
	var data_list: U#Sequence[T#Numeric#Comparable[T]]
	var data_map: V#Map[String, U]

	def process_data:
		for t in data_

implement Option[T#Stringable] is #Stringable
	def String:
		if self is 
			Some(v): "Option.Some({v.String})"
			None: "Option.None"

-- compare: 
-- implement[T: Stringable] Option[T] is Stringable

implement HashMap[K#, V#] is Map[K, V]

implement

```

Much more concise, definitely more readable than the triple type-arg version and arguably even more readable than the traditional syntax.

#### Downsides
- The actual number and order of type parameters may not be immediately apparent.  
	- This becomes important when you want to specify type arguments to a function call as a means of disambiguation of overloads.  
	- Ameliorated slightly by treating this as syntax sugar, and still allowing these declarations to be listed in square brackets if desired
	- and/or allowing type declarations in the body like `type T#Numeric#mut` so that `T` can then be used elsewhere thereafter
- I had another one... I can't remember it now

## Error Handling
## Concurrency

I'm designing and looking for feedback on this concurrency model for my experimental programming languages, called Mismo.  Particularly, I have these three questions:
1. Is it safe?  (from data-races and deadlocks)
2. Is it powerful?  (is this system going to help you or hinder you in making concurrent solutions)
3. Is it fun?  (would you enjoy thinking about concurrency problems through this lens)

Before I get to concurrency, I have to first give an overview of the memory model, because the concurrency model builds off that.

### Mismo Memory Model
Mismo has no garbage collection.  Instead, it leans into single ownership for memory safety.  An owned value, declared with `var` can be borrowed by functions (mutably with `mut` or immutably with `let`, rust-style) or it can be *moved* into the function (with the `sink` calling convention), in which case the value is no longer accessible from the caller's context.  Borrows can be re-borrowed, of course, and sometimes can be returned from functions (if the compiler can prove the borrow has a sufficient lifetime).  But these borrows cannot be stored in structs or arrays.  (Yes this is seriously restrictive, but we'll save Mismo's strategies for dealing with that for another day.)

This is all checked and enforced at compile-time.

#### Bindings / Reference Capabilities
In Pony:
- iso — unique, mutable, sendable
- trn — unique, mutable, aliasable
- val — shared, immutable, sendable
- ref — shared, mutable
- box — shared, immutable
- tag — opaque, sendable

In Mismo (current model)
- var — stack-allocated, owned, mutable, sendable
- mut — same as var, but with restriction that must be valid at the end of it's region
- let — immutable, shared, borrowed
- ref — ref-counted, heap-allocated, shared, immutable, sendable

Okay, let's take a look at all the dimensions and see which intersections we're missing.
- Thread-Safe — derived property; must be either unique, or globally immutable
- Uniqueness — no aliases
- Mutability — local and global (im)mutability
- Allocation — stack or heap
- Ownership — either owned + borrowing, or shared with ref-counting

What if we add one more binding, `box` to the list above, then what do our subtyping relations look like, and the properties of structs involving such bindings as fields?

Parameter Passing Conventions

| Convention | Accepts                  |
| ---------- | ------------------------ |
| move       | move                     |
| mut        | var, mut                 |
| box        | move, box                |
| ref        | move, ref                |
| let        | move, var, mut, box, let |

| Binding | Can pass as              |
| ------- | ------------------------ |
| var/mut | move, mut, box, ref, let |
| box     | mut, box, let            |
| ref     | ref, let                 |
| let     | let                      |

Note about passing a `box` as `mut`: this is pass-by-reference, but if the value itself is then moved (ie, by being passed to another function or field as `move`, `mut`, (or even `box` or `ref`?)) then a new value must be written back into the box.

Field bindings
- var
- box
- ref
- ...let?
	- but if you have a `let` field, then the whole struct has a limited lifetime
	- I guess the only way for this to really work out is to have lifetime annotation/analysis or something like that

`var` fields are assigned by `move` convention.  `box` and `ref` fields are assigned by `box` and `ref` conventions respectively.

Tips for choosing conventions for functions.  Prioritize in this order
- let — stick to this default if you only need to read the value, because any binding can be passed
- mut — a good default for any mutable data (vars and boxes)
- box, ref, move — lonly choose these convention if you need to store the value in a `box`, `ref`, or `var` field, respectively (otherwise, `let` or `mut` is probably a better choice)

Is this safe for structs that contain resources such as pointers, file-handles, etc?

Example, let's say we have a `box Array[String]`.  And we pass that as a `mut`... then we may end up storing that array somewhere else that the caller has no idea.  But that's okay, because as soon as I passed it as `mut`, I essentially gave permission to the function to do almost anything with it, including completely replacing it with another value.

So, what *safe* things can we *not* do with this set-up?
- if you want arbitrary shared mutability, use `box`.

**Do we really need box and ref?** 
- No, actually, they're just convenient
- another possibility, arguably simpler, would be to only have the bindings `var`/`mut` and `let`, and then have built-in classes like `Ref[T]` and `Box[T]`.
	- advantages:
		- simpler core semantics
		- easily extendable in case you want to do, eg, `GenerationalReference[T]`
	- disadvantages/problems to overcome:
		- `Ref[T]` and `Box[T]` have to be explicitly dereferenced every time you call a method
			- I guess trying it out in practice would reveal how painful that is in reality
			- might explore some kind of "method forwarding"... but might not be worth it
		- what about thread-safety?
			- `Ref` would be fine because it would call the `copy` method every time it's passed and just work as if it were a unique `var`.
			- but then `Box` could do the same thing... we don't want that
```
var x = some_constructor(args).Ref
x.mut.method
x.let.method
```


### Mismo Concurrency Model
Mismo adopts a variation of the actor model, inspired by Pony (which is admittedly the only concurrency paradigm I've every actually used).   Any type can be instantiated as an `Actor`.  Each actor gets it's own light-weight thread, ie, code within one actor always runs synchronously, has it's own isolated heap of memory.  An actor may have multiple "messages" queued, but only one at a time will actually run.

Owned values are equivalent to Pony's `iso`.

Quick primer on syntax:
- square brackets indicate type parameters/arguments for generic type/function
- leading underscore means "private field"
- `fn mut` declares a method that takes its receiver as a mutable borrow.

```
struct Actor[T, Ch]
	var _object: T
	var _chan: Queue[Either[{mut T => Nil}, Ch]]
	var _thread: Thread

	fn mut send(sink value: Ch)
		if self._chan.is_empty:
			self._thread.continue(value)
		else:
			self._chan.push(Either.Right(value))

	fn mut do(func: {mut T => Nil})
		func.call(_object)

```

```
 1 |  fn main
 2 |     var logger = Logger.new
 3 | 	 var async_logger = Actor[Logger, String].new(sink logger)
 4 | 	 # logger is no longer accessible, it's moved into async_logger
 5 | 	 async_logger.do({log: Logger => log.print_two_messages("hello")})
 6 | 	 print("interrupting cow")
 7 | 	 async_logger.send("world")
 8 | 	 print("says moo")


struct Logger
	fn mut print_two_messages(first: String)
		print("I have two things to say:")
		var msg = first
		msg += " and "
		msg += await String
		print(msg)
```

That program should print out the following:

> interrupting cow
> says moo
> I have two things to say:
> 1) hello
> 2) world

or

> interrupting cow
> I have two things to say:
> 1) hello
> 2) world
> says moo


Let's examine some of these lines in more detail: 
```
 3 |  var async_logger = Actor[Logger, String].new(sink logger)
```
This moves the newly created logger into an Actor with type arguments `Logger` and `String`.


#### Alternative Models
##### Different types of actors
Make the Actor type a trait, then have actor versions with and without channels.
```
trait Actor[T]
	fn _object T
	fn _thread Thread
	fn mut do(func: {mut T => Nil})
		execute in _thread: func.call(_object)

struct ActorWithChannel[T, ]
	
```


##### Actors + Channels and Promises
```
fn main
	var chan = Channel[String]
	var logger = Actor(Logger.new(ref chan))

struct Logger
	ref chan Channel[String]
	
	fn mut print_two_messages(first: String)
		print("I have two things to say:")
		var msg = first
		msg += " and "
		msg += chan.await
		print(msg)	

struct ref Channel[T]
	var _queue Queue[T]

	new (cap Option[Int] = Option.None):
		Channel(Queue[T](cap))

	fn .await T:
		# pause the current thread until a value is available in the channel
		# while waiting for a value, the thread may continue processing 
		# messages in the message queue

	fn .send(value: T):
		for t in MISMO.threads.iter_mut:
			if t.idle and t.
		else:
			._queue.push(value)


```

##### Actors + Coroutines
I really like the ergonomics, power, and clarity of python's generators. But to base Mismo's concurrency on such coroutines becomes a little messy, I think — primarily because of "function coloring" that people seem to dislike.

I also really appreciate the actor model (which I learned through Pony lang). Although reference capabilities are a high learning curve, the actor model itself is really intuitive, ergonomic, and powerful (though maybe not quite as powerful as Go's goroutines).

So how can we combine them?

```mismo
actor Fib
	constructor: -- or maybe not
		var a = 1
		var b = 1
		yield a
		while true:
			a, b = b, a + b
			yield a
			
	def mut fib Int
		var a = 1
		var b = 1
		yield a
		while true:
			a, b = b, a + b
			yield a

function main:
	var fib = Fib
	Fib.fib  --> 1
	Fib.fib  --> 1
	Fib.fib  --> 2
	Fib.fib  --> 3
	Fib.fib  --> 5
	-- or
	Fi
```


## Modules
### Importing from Standard Library
```mismo
import std
	math
	io
	collections/map
import src
	utils/tool
	services/auth
import external
	github.com/author/lib
```

### Project Structure
Running the `mismo` command without any arguments will search for a `src` directory in your current directory.  All of the files in that directory that end with a `.mismo` file extension will be compiled concurrently in the same namespace.  

Let's say your project is located in `/home/ryan/mismo_projects/hello_world`, and that directory has the following structure:
```
awesome_project/
├─ test/
├─ src/
│  ├─ main.mismo
│  ├─ services/
│  │  └─ auth/
│  │     ├─ foo.mismo
│  │     └─ bar.mismo
│  └─ utils/
│     └─ tool.mismo
└─ README.md
```

Now let's say within the deeply nested `foo.mismo` and `bar.mismo` files there exist types `Foo` and `Bar`, here are how you would import them (the syntax does not depend on the location of the file containing the import statement, as long as it is being compiled in the default way as mentioned above.)
```
import src:path
```

```mismo
import path/to/module {Foo, Bar}
```
This is the most usual way to do imports.  Now `Foo` and `Bar` can be used in this namespace as is.

```mismo
import path/to/module/foo.mismo {Foo}
import path/to/module/bar.mismo {Bar}
```
Specific files can also be imported, avoiding the need to process all `.mismo` files in a given directory.

```mismo
import /path/to/module
-- or
import /path/to/module as mod
```
This is a namespaced import.  All types within that module are usable with dot syntax, eg `module.Foo` and `module.Bar`.  The name of the module, by default, is the name of the last directory in the path (in this case "module") but to prevent naming conflicts you may append `as <module name>` to use the types like `mod.Foo` and `mod.Bar` (in the above example).  

```mismo
import /path/to/module/foo.mismo
```
If a file is imported as a module, the same rules apply except the `.mismo` extension is stripped.  In this case, the `Foo` type would be used like `foo.Foo`

```mismo
import path/to/module as mod
```

Whole paths are also supported.  Those paths must start with a `/`.  If your path contains whitespace or the `{` character, they must be escaped (eg, `import hello\ world {Type}`) or the path can be quoted (eg, `import "hello world" {Type}`)

#### Alternate `src` directory
Alternatively, you can also supply a path as an argument to the `mismo` command and that will then become the relative directory for resolving import paths.  You can even specify one specific file in order to ignore other `.mismo` files in the directory; in that case, import paths will be relative to the directory of the specified file.

# Implementation
## Parsing
### peek_indent

peek_indent method that gemini wrote — I'm pretty sure it's completely irrelevant, but just in case:
```crystal
  # --- Indentation ---
  # Gets the indent level of the next significant line.
  # Call when peek is a Newline.
  private def peek_indent : Int32
    # We assume peek is a Newline. We want the indent of the *next* line.
    # The Token::Newline itself should store its *own* line's indent.
    # So, if we are AT a newline, its `indent` field is what we need.
    # If the Pony version `tokens(index + 1)?.loc.col` meant the token *after* the newline,
    # then we need to find the first non-whitespace token on the next line.
    # Given user's update: "Token::Newline struct to store the indentation as a field"
    
    # Let's find the next non-comment, non-empty line's first token's indent.
    # This is more robust than just looking at the token immediately after a newline.
    temp_index = @index
    while temp_index < @tokens.size
      tok = @tokens[temp_index]
      if tok.is_a?(Token::Newline)
        # The indent field of Token::Newline is the indent of *that* line.
        # If this newline is followed by only whitespace/comments then another newline,
        # we care about the *next* actual content.
        # For now, let's assume Newline.indent is the one we want for the line it represents.
        # If `peek_indent` is called *on* a newline, it means the indent of the *next* line.
        # So we need to look past the current newline.
        if temp_index == @index # If current token is newline
          # Look at the indent of the *next* line
          (temp_index + 1...@tokens.size).each do |i|
            next_line_tok = @tokens[i]
            # Skip blank newlines or comment lines to find the true next indent
            return next_line_tok.indent if next_line_tok.is_a?(Token::Newline) && next_line_tok.indent >= 0 # Valid indent
            # This part needs more robust logic if comments are tokens and need to be skipped.
            # For now, assume Newline.indent is what we need.
            # If the *next* token is Newline, that's the one.
            if @tokens[i].is_a?(Token::Newline)
              return (@tokens[i] as Token::Newline).indent
            elsif !(@tokens[i].is_a?(Token::Comment) || @tokens[i].is_a?(Token::Whitespace)) # First non-comment/whitespace token
              return @tokens[i].location.column # Fallback to column if not newline
            end
          end
          return 0 # EOF or only comments left
        else # If we are looking ahead past some other tokens
          return tok.indent if tok.is_a?(Token::Newline)
        end
      end
      temp_index += 1
    end
    0 # Default if no newline found or at EOF
  end
```

### ast nodes and hir nodes
Simplified AST:
- value literals:
	- nil
	- false
	- true
	- int
	- float
	- string
	- array
	- tuple
- binop
	- eg `4 + 5`
	- includes assignment (`let variable = value`)
	- does not include commas, semicolons, dots
- identifier
- call
	- `function(args)`
	- `obj.method`
	- `.property`
	- `.call(args)`
	- `obj.method(args)`
	- `TypeName`
	- `Constructor(bind)`
	- `Static.call`
	- I think it makes the most sense to group these as follows:
		- FunctionCall: `function(args)`
		- MethodCall: `obj.method`, `.method(call)`, `Static.call`, etc
		- Constructor: `TypeName`, `Constructor(bind)`
		- on second thought, maybe I don't need that MethodCall (I have decent logic for combining that with FunctionCall)... but what to do about that `Static.call`?
			- could make it a method call where the first argument node is of type `Ast::Constructor` or even just `TypeNode`
				- but that makes it slightly awkward to lower to IR because then you have to check for that case again
			- so I guess it does deserve to be its own thing
		- maybe we just need to separate out the `Constructor` from `FunctionCall` then — because it serves a different purpose in match branches.

### UCS (Ultimate Conditional Syntax)
The way parsing UCS was supposed to work in the Pony version of the Mismo compiler was to have the expression parser explicitly pause at boolean operators (`>`, `==`, `is`, etc) and `and`, then potentially branch from there.

NOTE: we may have to change this up for parsing UCS nodes. Having the parser stop at boolean operators and/or 'and' tokens messes up the logic for operator precedence. In particular, it essentially lowers the precedence of these stoppers below all other operators in the context of a UCS node. This is inconsistent and potentially confusing. 

***So what do we do instead*?** 
One possibility is we replace those two variants with a `StopAt` variant that tells the parser to parse normally until it hits an indented line, and stops *only* if the last token was a boolean operator or an `and`. 

- Oh, I just realized that this is also gonna require the peek-two-ahead capability! I mean, if we want the operator to stay in the queue. 
	- the other thing that could really use the peek2 method is finding an `else` token after an outdented newline
- Alternatively, we could also return that operator token to the caller... but that would require modifying the call sites of Expression#parse. 
- Another potential fix for this problem is to have a Ast::UcsBranchPlaceholder node that we can insert into the AST to represent the branch. Then we can just parse the branch as a normal expression and insert the placeholder into the AST. 
	- *But then at what point do we actually replace the placeholder*? 
	- We could do it while parsing the UCS. Will require some fairly simple recursion. 
	- Or we could leave it until the HIR lowering phase.
- OR we could solve it at the **lexer level**: have the lexer track it's context (in or out of a UCS node) and emit a different token to replace the boolean operator + indented newline combination
	- that would be the best from the perspective of the parser, but it does add some complexity to the lexer

Okay, I think the simplest fix for this is to enable a `peek2` method via collecting 
## Functions that don't compile
```mismo
fn foo
	let a = 1
	a = 2
	-- cannot reassign a let-binding
```

## Operator DAG
```
	1 + 2 * 3
=>  1 + (2 * 3)
```

## Compiler Stages
- raw text
- Lexer -> token stream
- Parser -> AST
- Type Checker -> HIR
- .... -> MIR
- Optimizer -> Bytecode


Token Stream
- one dimensional (but it does still check for matching quotes, braces, etc)
- things like Number literals, dot character, operators, identifiers, block start/end, etc

AST
- tree structure
- nodes are statements, expressions, and blocks
- things like string literals, identifiers, blocks, binary expressions, dot-expressions, etc

HIR
- like AST, but contains type information
- Expression: {Position, TypeState, Expr}
- TypeState: {Access, Type, Lifetime}

After parsing the full AST, the compiler receives all the declarations.  Before type-checking, the compiler will first flesh out the TypeEnvironment by converting type declarations into (Base) structs, enums, and traits.  

Then the compiler will also check that all trait implementations are satisfied.
- for each type (struct or enum), check that it satisfies each of the traits it listed (including parent traits)
- for each method in the trait, check that a compatible method is implemented
	- if not, and if the trait supplies a default body, then generate a new method from that

Then the compiler checks for function overload ambiguities.  For each function, analyze each pair to make sure that no ambiguity is possible.

Finally, typechecking phase.  Each function is sent to a TypeChecker which takes the AST of the function body, checks for type errors, and lowers the body into IR.  During this phase, the TypeCheckers will also send generic type and function instantiations back to the compiler to be cached for the next stage.

Then there needs to be at least one more stage after this to determine function overloads for each call in each function instance.  Turns out that because of generics, this can't always be done at the typechecking phase


### Brainstorming about instantiating types


```
function average 
	fn [T](numbers Array[T]) T: ... -- Overload A: Generic average 
	fn (numbers Array[T]) Float: ... -- Overload B: Specialized average for Ints

-- with traits
trait Averageable
	fn average(numbers Array[Self]) Self: 
		... -- default implementation

extend Int is Averagable
	fn average(numbers Array[Int]) Int:
		... -- specialized implementation 



-- print list example
-- Generic function to print elements of an array
function print_list[T Stringable](arr Array[T]):
	...

print_list([1, 2, 3])
print_list(["hello", "world"])


-- can we get the same effect with traits?
trait PrintList[T Stringable]
	fn .print_list:
		...

extend Array[T Stringable] is PrintList[T] if T is Stringable
-- alanganin ito!
-- this looks really complex to implement
-- and it would still require an explicit `extend` declaration for each type

trait PrintableInList
	fn print_list(self Array[Self]):
		...

extend Int is PrintableInList
	fn print_list(self Array[Int]):
		...



```


### Generic Traits
```
trait KeyValue[K, V]
	fn get(key K) V

struct Array[T] is KeyValue[Int, T]
	fn get(index Int) T
```

In the above example, how would the compiler go about confirming that `Array[T]` actually satisfies `KeyValue[Int, T]`?

So, `KeyValue` is a TraitBase, but `KeyValue[Int, T]` is an instantiated Trait (although it is still abstract due to the use of the type variable T).

`KeyValue[K, V]` is looking for method `fn get[T1, T2](_ T1) T2`, therefore `KeyValue[Int, T]` is looking for method `fn get[Int, T](_ Int) T`, which is equivalent to `fn get[T](_ Int) T`.

Testing for method equivalence in this context 


...
### Non-expression nodes at various levels
AST produces:
- StructDeclaration
	- FieldNode
- EnumDeclaration
- MethodDeclaration
	- TypeParameters
	- Parameters
	- ReturnType
- TraitDeclaration
	- MethodTemplate — I guess this can be the same structure as a MethodDeclaration, since it never actually becomes a real method without being instantiated anyway
	- ... except it needs an optional function-body...
	- and `Self` could actually just be a type-parameter

StructDeclaration -> StructBase
EnumDeclaraiton -> EnumBase
MethodDeclaration -> MethodBase
TraitDeclaration -> TraitBase
MethodTemplate -> MethodBase

So, the really confusing thing is generic traits.  Generic types are fine to implement traits, they just require generic functions

```
trait Trait
	fn trait_foo(a Int, b String)

trait GenericTrait[A, B]
	fn trait_foo(a A, b B)
	-- AST produces TraitMethodDeclaration:
	-- - trait_foo[A, B, Self GenericTrait[A, B]](a A, b B)
	-- the body of this method is type-checked as is, 
	-- and saved to be instantiated in the stage after the IR
	-- TypeEnvironment should convert this to a validated signature (just once, so as not to validate it every time it checks for trait satisfaction)
	-- then when GenericTrait is being tested for type `Generic[X, Y] is GT[Y, String]`, 
	-- trait_foo needs to be (partially) instantiated according to the declaration: 
	-- A=>Y, B=>String, Self=>Generic[X, Y]
	-- and this instantiation needs to happen in a TypeScope with X and Y
	... okay this part is confusing... do I need a double-layered TypeScope?

struct Type is Trait
	fn trait_foo(a Int, b String): ...

struct Generic[X, Y] is Trait
	fn trait_foo(a Int, b String): ...

struct Type is GenericTrait[Int, Float]
	fn trait_foo(a Int, b Float): ...
	-- in this case, GenericTrait has been instantiated at the point of declaration, 
	-- and therefore the required method has also been "instantiated", but not because
	-- of a callsite, the generic function never gets constructed.
	-- in other words, the [A, B] generic parameters have been removed

struct Generic[X, Y] is GenericTrait[Y, String]
	fn trait_foo(a Y, b String): ...
	-- the implied generic parameters will *always* be a subset of the type parameters 
	-- of the struct, not of the generic trait

"""
but the template of the method can be typechecked by adding an additional type Parameter `Self` — but the IR of the body needs to be modified after being checked to replace the `Self` and other generic parameters if they exist.
"""
```


What even *is* a TypeScope?  Currently, the interesting property is `type_parameters: Map[String, TypeParam]`... but why do I need to map `String`s to `TypeParam`s?

Ah, a TypeScope is for evaluating TypeNode expressions where an expression like `T`  would normally cause an error, instead it produces a TypeVariable.  Looks like this is the sole purpose in the existing code.  I guess then the map should actually be a map to type *variables*, so those can be reused.  Ie `let type_parameters: Map[String, TypeVariable]`

So, when testing if a trait method is implemented for a given type, you gotta do this:
- prerequisite: all types, traits, methods, and method-templates are validated using their respective TypeScopes and converted to their validated forms (without the `Declaration` suffix)
- for each trait of each type:
- calculate the *replacement rule* from the declaration 
	- `struct Generic[X, Y] is GenericTrait[Y, String]`
	- => {`A=>Y, B=>String, Self=>Generic[X, Y]`}
	- this rule should be represented as a map, probably of the form `Map[String, Type]` (or maybe even `Map[TypeVariable, Type]`) to make these substitutions
- take the method (eg `trait_foo[A, B, Self GenericTrait[A, B]](self Self, a A, b B)`)
- **replace** the types in the parameters according to the replacement rule
	- => `trait_foo(self Generic[X, Y], a Y, b String)`
- then **rebuild** the type parameters by counting up the type variables in the signature
	- ... I guess this will *always* be the exact type parameters of the implementing type?  Unless the trait defines a required method with additionol type parameters, I guess
- *then* look to see if that signature is present

```
trait Trait[X]
	fn foo[Y](x X, y Y)

struct Type[A] is Trait[A]
	fn foo[Y](x A, y Y): ...
	         
```