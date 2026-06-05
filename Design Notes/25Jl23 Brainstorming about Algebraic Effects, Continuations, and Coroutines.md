---
created: 2025-07-23 14:48
related:
  - "[[Mismo]]"
---
#2025/Jul/23 #programming 
***

The other day, I had a thought: interfaces/traits can be unified with algebraic effects.  [ChatGPT thought it was a good idea](https://chatgpt.com/share/68808606-12e4-8010-8b75-eca97a872dcc) for [[Mismo]].

But before committing to that, I should think hard about whether effects are even worth it or not.  [There is criticism](https://www.reddit.com/r/ProgrammingLanguages/s/CXOGmIfclj).  I definitely do not completely understand the pros and cons, but here's my attempt at summary in general:
- pros
	- a unified platform for exceptions, coroutines, and more
	- more expressive user-definable control-flow
- cons
	- harder to reason about — especially undelimited continutations
	- additional dimension to function signatures (cognitive load)
	- the code to perform effect handling is usually not that readable
		- (that's probably a syntax or language specific thing, not sure if it's true in general)
	- using generators/exceptions/async/etc -specific features lends more clarity as to the intent of the code
		- in other words, may be useful for implementing other language features, but not that useful as a feature unto itself

So that's theory, an in practice, apparently, people just simply don't use continuations all that often, indicating it might not be all that useful in practice.

So what do I actually want out of algebraic effects, and are effects indeed the best way to get it?

- **Traits / Interfaces**
	- after exploring this a bit, it is slightly unnatural to have scoped interfaces, but I think it's worth a shot to see how it plays out in practice
- **Coroutines**
	- although I don't grok it fully yet, it seems effects can indeed model coroutines just fine
	- especially with first class continuations
- **Exceptions**
	- I don't need this, Result and Option are good enough for Mismo — although I would use it if we decide to go all in with effects
- **Context Propagation**
	- ante blog gives an argument for this, but to me it seems like a very minor gain
	- usually, the context object should just be the first `self` argument to the function, then you can omit it in Mismo with the syntax sugar `.method`
	- wild idea: what if you want *two* context objects?  Can you write a double-object method?
		- too wild for now
- **Concurrency**
	- async/await, threads/fibers
	- I guess this is in the coroutine camp
- **Callbacks / Closures**
	- well, this is an easy one... any language worth its salt nowadays should have closures anyway
- **Backtracking / Nondeterminism**
	- this one is interesting, effects can do it well, but it still seems like the wrong way to do it for me
	- I feel like this pattern is better coming from a library, rather than somewhat magical "effects"
- **Capability Based Access**
	- good for IO, database access, filesystem, etc
- **Dependency Injection**
	- really useful for testing, mocking, user mode vs auto mode, etc
	- this can also be done with regular traits

This is quite the list.  I think I can achieve all of these with **interfaces** (either scoped interfaces or just regular traits) and first-class stackless **coroutines**.  (And sum types for error-handling.)  And I think the coroutines can also sub in for closures as well.


```mismo
interface Stringable[Self]
	def string(self Self) -> String

interface IO
	def print[T](msg T) with Stringable[T]
	def input -> String

struct Point .Stringable .Addable
	var x Int
	var y Int
	def string: "(${x}, ${y})"

provide Stringable[Point]

def main with IO
	let p = Point(1, 2)
	print(p)
	print(334) with Stringable[UInt]
		def string(x): x.hexadecimal
	print(334) with string(x): x.hexadecimal
```

I guess you don't need the `with IO` if `IO` is provided globally.



```mismo
def lexer(reader Reader) -> Coro[Token, Nil]:
	\() => 
		while true:
			if reader.next
				is Some(char) and char
					== '(': 
						yield from handle_paren(reader)
					== '[': 
						yield from handle_bracket(reader)
					.is_number:
						yield Token.Number(char)
					.is_operator:
						yield Token.Operator(char)
					.is_whitespace:
						continue
					...
					
				is None: 
					break
		while true:
			yield Token.EOF

def parser(text String):
	var lexer = lexer(Reader(text))
	var tok = lexer.call
	if tok is ...

	
	
```


In order for a `Coro` to act as a closure or as an infinite stream, the compiler must be able to determine the function never terminates.  The last statement must be an infinite loop.

If the function does terminate, then there ought to be some interface the caller can observe to be informed of that.  Or maybe the closure should automatically restart?  Nah, that could lead to issues with consuming vars twice and such, it's gotta be either an explicit loop, or an explicit "coroutine exhausted" thing.  Or, simply distinguish between a `FnOnce` and a `Coro`.  Or maybe call it a `Thunk` or `Computation`...


### About Scoped Interfaces
Inspired by effects being able to represent traits, I thought about this idea of "scoped interfaces" — which is essentially the subset of effect handling that models traits/interfaces.  It would work like this: 
- declare interfaces (possibly with a `Self` type parameter) at the top level
	- eg `Log` or `Stringable[Self]` which require functions like `def log(msg: String)` and `def string(self: Self)`
- functions definitions can *require* certain interfaces to be implemented (as part of their signature)
	- eg syntax: `def print[T](item: T) with Stringable[T]`
- function calls *provide* implementations to required interfaces
	- eg `print(foo) with def string(self: Foo): "I'm a foo"`
- to keep things ergonomic, we should also allow *global default implementations* of interfaces; any function that may require such interfaces may omit the `with Interface` syntax
- alternatively, could put it all in `main`...
```
def main with IO, Stringable[Int], Iterable[Array], etc with
	def print[T](msg: T) with Stringable[T]: ...
	def string(self: Int) -> String: ...
	def ...:
	-- now we finally get to the body of main here
```
- yeah, that's a little too much

Anyway, it's an interesting idea, but I'm not sure if it's any more useful that regular traits.  I mean, if you ever to change the implementation for a certain slice of the call stack, you can usually use wrapper types.  Well, I guess this only works for generic types, actually.
```
trait Fooable[Self]
	def foo(self: Self) -> Foo

def make_foo[T: Fooable](item: T) -> Foo
	item.foo

def main:
	let bar = Bar
	bar.make_foo

-- make a wrapper for bar to modify the behaviour
struct BarWrapper is Fooable
	var bar: Bar
	def foo(self: BarWrapper) -> Foo
		print("And that's a wrap!")
		bar.foo

def test:
	let wrap = BarWrapper(Bar)
	wrap.make_foo
```

But I guess you can't actually do it for non-generic types, eg, imagine if `make_foo` took a `String` rather than `T`; you can only implement `Fooable` once for `String`, so if you want to add modify that behaviour just for one slice of the call stack, you would have to refactor to use generic code.


### Coroutines and Closures
Rust has three traits that closures automatically implement: `FnOnce` (for moved values), `FnMut`, and `Fn`.  They are implemented hierarchically as well, so any type that implements `FnMut` also implements `FnOnce`, and any type that implements `Fn` automatically implements all three.  Polymorphism ftw.

I think Mismo could use all three of those, potentially, as well as their counterparts for Coroutines.  Except "CoroutineOnce" doesn't make any sense.
- `FnOnce`
- `FnMut`
- `Fn`
- `Coro`
- `CoroMut`

or we could really simplify it unify into just one trait: `Callable`, and the built-in types `Coroutine` and `Closure` would both implement that trait.

```
- CallableOnce
- CallableMulti
- Subroutine
- Coroutine
- Procedure
- Proc
- Executable
- Thunk
- Computable
```
- CallableOnce
- 
```
trait FnOnce[Out, In...]
	def call(...args: In) -> Out

enum CoroutineState[Y, R = Nil]
	Yielded(Y)
	Complete(R)

trait Coro[Y, R = Nil, In...]
	def next(...args: In) -> CoroutineState[Y, R]

extend[Y, R, In...] Coro[Y, R, In] is FnOnce[CoroutineState[Y, R], In]
	def call(...args: In) -> CoroutineState[Y, R]:
		self.next(...args)
```