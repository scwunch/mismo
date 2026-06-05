---
created: 2025-11-15 22:36
tags:
  - programming
related:
  - "[[Mismo]]"
---
[Link to conversation with ChatGPT](https://chatgpt.com/c/69186d7a-8084-8323-b051-3a78bed7879e)

## The Problem 
[[Mismo]] supports both type-based function overloading AND generic functions (and types) via type parameters.  I like both of these features, and I'm not yet willing to give up either of them, but they are not completely orthogonal and can occasionally interfere with one another.The primary interference I'm thinking about is *ambiguous functions calls*.  If we *don't* have generic functions, but we support overloading, then the only way to get an ambiguous function call is to essentially duplicate the exact same function signature (note Mismo doesn't support sub typing).  That kind of issue is very easy to debug and fix, so not really a problem worth talking about.  However, once we add generics in there, and also the ability to *infer* type arguments to functions, then ambiguous functions get much trickier.Here's an example:

```
fn foo[T](arg: T) -> T {
    return arg
}fn foo(arg: String) -> String {
    return "hello " + arg
}
fn main {
    foo("string value")
}
```

Both overloads of foo. The ambiguity is the problem.

## Potential Solutions
### 1. Generic XOR Overloads
- Since the problem arises from the intersection of these two features, make them mutually exclusive
- a given function can be implemented EITHER generically OR multiple overloads
- **BUT**: now I can't even overload methods of generic structures (like `fn to_string[T](Array[T])`)

### 2. Prioritization by Specialization
- introduce a few (or lots) of rules to determine how "specialized" a given call is
- for example, an overload with fewer type parameters might be more specific and have higher priority than other overloads
	- also a type parameter with a constraint would be more specific as well
- could also look at number of arguments, variadics, default arguments
- actually, a better method would be looking at the specialization of each argument type
	- eg, `Array[Int]` < `Array[T]` < `T`
	- but this on it's own is not enough, still need more rules/strategies for further disambiguation
- **BUT**: too magical, not predictable enough

### 3. Prioritization Annotations
- for example, this could be through an annotation like @override on any function that would then take priority over functions without it. 
- would be useful for overriding a generic function imported from a library 
- Similarly we could also allow @fallback for the opposite effect 
- This allows the developer to control the prioritization buckets 
- ... but increases language complexity and learning curve - and then where does it stop? What if I then want to override an override? Maybe I just need annotations like @priority(7) ... but this is getting to be overkill 

### 4. Compiler checks for ambiguity of functions *by their definition* (not at callsite) 
- this gives the developer confidence that function overload errors will be caught early 
- but increases compiler complexity; I'm not sure this is even possible to check 
- and disallows the arguably helpful pattern of generic fallback + concrete specialization overload 

### 5. Metaprogramming compile-time switch (ala Zig) 
- within the body of a generic function, allow some kind of if T is Int then do this else do that such that when said function is monomorphized, the compiler just picks the right code path to code gen. 
- *requires a meta-programming language* (which may or may not be a good thing, depending on who you ask) 
- still can't specialize functions you didn't write yourself (eg imported from library)

## Evaluation of Solutions
1. **❌** totally breaks Mismo's model of overloads-as-methods
2. **❌** Depending on the version, it's either *too magical*, or not powerful enough (still requires some other form of resolution)... and in some cases might even resolve quite wrongly
3. **❌** this one is tempting because it allows you to specialize overloads over imported generic function... however, it has lots of cons:
	- too easy to abuse
	- adds a "pro feature" to the language (not friendly to learners)
	- without `@priority(n)`, still an incomplete solution
	- with `@priority(n)`, just ugly design
4. This one is very tempting as well... but the implementation complexity is daunting, maybe even impossible
5. This one is nice, but doesn't solve the cross-module problem.  Not a really great general solution, but certainly worth consideration whenever I get around to designing and implementing metaprogramming

## So let's explore Solution 4 a little more in depth

a decently simple version of this strategy I think would be good to issue warnings (probably not errors)
- for each overload of a named function, compare it to all other overloads like this:
	- if arity is different, proven disjoint
	- if concrete type constructors of any two corresponding parameters are different, proven disjoint
	- if one or both of the parameter types are type variables, then disjointness cannot be proven
- Note: we should not try to prove disjointness by trait constraints on type parameters, because it will always be possible for a type to implement both traits in downstream code, therefore satisfying both interfaces
- *unless* we allow negative trait bounds...

Note that this is not actually an overload resolution strategy, rather a strategy for detecting ambiguities earlier.  Which is also helpful.  But it also needs to be paired with another strategy to disambiguate.  So that could be:
- same disambiguation strategy: raise error and ask developer to specify type arguments
- `@override`-like annotations
- disallow overloads that can't be proven disjoint

I think I like that last option... even though it might be restrictive.  But it can then be further enhanced by a Zig-like specialization mechanism.

Speaking of which, here's another thought: 
```libA
fn some_function[T](arg: T) =>
	-- implementation
```
```main
import libA { some_function }

@specialize
some_function[Int] =>
	-- specialized code for when T=Int
	
```

This would be like an ad-hoc version of the Zig-like compile-time switch, with all the benefits plus it can work cross-module

### In other words, *We still need* some *way to specialize a generic overload imported from another module*
- just a matter of finding good syntax for it
- or settling for some kind of workaround

A decent workaround I can think of us is this: taking the above example of `some_function`:
```mismo
import libA { some_function as _generic_some_function }

fn some_function[T](arg: T) {
	#if T == Int:
		-- specialized code here
	#else:
		-- generic fallback
		_generic_some_function(arg)
}
```

Now that I've written this out, I highly doubt this will be a common-enough pattern to warrant dedicated syntax sugar or a dedicated feature.

## Reddit Post
Title: Function Overload Resolution in the Presence of Generics

In Mismo, the language I'm currently designing and implementing, there are three features I want to support, but I'm realizing they don't play well together.  
1. Type-based function overloading.
	- Early on I decided to experiment: what if we forego *methods* and instead lean into type-based function overloading and UFCS (ie `x.foo(y)` is sugar for `foo(x, y)`)?
	- Note: overload resolution is purely done at compile time and Mismo does not support subtyping.
2. Generics
	- specifically parametric polymorphism 
	- too useful to omit
3. Type argument inference
	- I have an irrationally strong desire to *not* require explicitly writing out the type arguments at the call site of generic function calls
	- eg, given `fn print[T](arg: T)`, I much prefer to write the call `print(students)`, not burdening developers with `print[Map[String, Student]](students)`

The problem is that these three features can lead to ambiguous function calls.   Consider the following program:
```
fn foo[T](arg: T) -> T:
    return arg

fn foo(arg: String) -> String:
    return "hello " + arg

fn main():
    foo("string value")

```

Both overloads are viable: the generic can be instantiated with `T = String`, and there’s also a concrete `String` overload.

**The question:**  
What should the compiler do?

Just choose a match at random?  Throw an error?  I'm hoping a smarter answer is possible, without too much "compiler magic".

What approaches have worked well in practice in similar designs?  Or is there a creative solution no one has yet tried?