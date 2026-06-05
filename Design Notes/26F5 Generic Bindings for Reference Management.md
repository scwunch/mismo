---
created: 2026-01-02 21:37
related:
  - "[[Mismo]]"
tags:
  - programming
---
A big part of what makes Mismo memory safe is treating references as second class citizens.  It's much easier to validate your pointers when functions do not return references and structs never store them.  Of course that's not strictly necessary for memory safety.  You can still have a system that guarantees memory safety while also allowing the storage and return of pointers.

## Why do we need `let`/`mut` fields?

*Why do we need this?  Because we want to return mutable references from containers, for one.  And for another we want to allow iterators to store references to containers.*

Alternatives?

For iterators that do not take ownership of their container, one way to work around this is to have the iterator store it's own state, but always pass a borrow of the container to the `next` function.

```
struct ArrayIterator[T]
	var index UInt = 0
	
	fn mut next(array: Array[T]) Option[T]:
		array.get(.index += 1)
		
fn main:
	var array = [1, 2, 3, 4]
	var iter = array.iter
	iter.next(array)
```

That's certainly possible, but decoupling the iteration state from the iterated container seems like a recipe for sync errors.

You could also sink the container into the iterator (and still retain a `mut` of the container if you really need it in the meantime) ang get it out again after.

What about returning `Option[let T]`?
- special case Option and Result (or monads in general?) in the compiler 
- allow the pattern only if it is immediately destructured in the caller
- pass everything in via closure
```
struct Array[T]
	fn get[R](index: UInt, closure: Fn[T, R]) -> Option[R]:
		if index < .size:
			Option.Some(closure.call(.slice[index]))
		else:
			Option.None
			
fn main:
	var array = [1, 2, 3, 4]
	var res = array.get(2, |elem|: 
		... do something with elem...
	)  -- type of res is Option[R]
```

***
One such system is lifetime annotations ala Rust.  But we want to avoid the cognitive burden that comes with that.

This note is an exploration of another potential system.

## Idea 1: implicit generic bindings
The idea is to let every type definition be generic over the "binding" of each field (ie, whether it is `var` , `let`, `mut`, etc).  This could be done implicitly (eg instead the field syntax `var foo Type` it could simply be `field foo Type` or even just `foo Type`) or it could be done with constraints on what bindings are allowed (eg `mut` could be `mut` or any binding that coerces to `mut`).  

Previously I was wondering what to do about the binding of variable types like the `T` in `Option[T]`.  In particular I wanted to allow the return type of `get_mut(Array[Foo])` to be `Option[mut Foo]` or `mut Option[mut Foo]`.  But this proposal recognizes that the generic binding pattern is useful not just for generic types, but also concrete types.  Like the iterator example we saw earlier.

### Implications for Type Checking 
Now every struct like 
```
struct Foo
	var bar Bar
	var baz Baz
	var qux Qux
```
is now
```
struct Foo[b1, b2, b3]
	b1 bar Bar 
	b2 baz Baz
	b3 qux Qux 
```

... This almost looks like lifetime variables...

But remember these are implicit and not directly observable or writable.  Of course the type checker has to know, and the specializer. 

The type checker will check if any of b1, b2, b3 are "second-class" (ie let/mut), and then only allow a value of that type to be returned from a function if it can guarantee a long enough lifetime.

And what if you want to place a value of this type in another struct or container?  Oh well I guess that container needs those binding variables too.  This might get out of hand quickly.  Every type is going to have as many binding variables as fields + fields of each field, including the ones behind a pointer.  What about for recursive types?  Like `Node[T] {data: T, children: Slice[Node[T]]}`.  The NodeT in the Slice may actually be different than the parent NodeT.  I think this may be impossible to check, at least at compile time.

Another implication: we now need bidirectional type inference.  Since we cannot annotate these binding variables, they must be inferred, always.  And now all of a sudden a function signature is no longer enough, we need some kind of global type inference algorithm.

**This is quickly becoming a bad idea both for the implementation and also for the error messages.**

*Is there a more controllable version of this we could try?*

## Idea 2: Explicit Generic Bindings
Maybe just don't make the generic bindings implicit.  Allow the developer to specify precisely when a binding is generic or not, and if so, what the constraints are.

For example:
For type parameters:
- a bare type parameter means it may be substituted with any binding
- `var T` means that the type must be a `var`
- `mut T` means the type can be `var`, `mut`, or `box`
- `let T` is equivalent to `T`
- (alternatively we could do `mut+` and `let+` for the generic versions for even more control)

And for fields the same syntax but with the name of the field inserted between the binding and the type name.

Now when referring to a type (eg, writing the return type of a function), of that type has generic bindings, how do we specify that?  Because I think we learned in the last section that it needs to be specified in function signatures.

How about `Foo[mut]` and `Foo[var]`?  Assuming Foo has one generic field binding, and no type parameters.  And the declaration would be:
```
struct Foo[a]
	a foo Type
	var bar Type
```

Okay, so how do we syntactically refer to the generic binding of a generic type parameter?
```
-- explicit separation of binding and type
struct Bar[a, T]
	a bar T
	ref name String
	
-- lowercase of type parameter 
struct Bar[T]
	t bar T
	ref name String

-- inseparable from type parameter 
struct Bar[T]
	.bar T
	.name ref String
	-- use a dot to indicate field name, and the whole type name comes after, including binding
	-- if using this syntax, we can still make *parameter passing conventions* prefix, as that is actually a slightly different concept
```

- Option 2 is a little two janky... 
- Option 1 offers the most flexibility and explicitness, with the cost of slight verbosity.  However, when would this flexibility actually be useful?  
- Compare to Option 3, where binding is always associated with type variable: When would you want to refer to the binding of a type variable *outside* of the type itself?

```
struct Foo[a, T, U]
	a bar Bar
	a baz T 
	a qux U
```

without that explicit binding variable, maybe you can actually still do this:
```
struct Foo[a, T: a, U: a]
	.bar a Bar
	.baz T 
	.qux U
```

```
Foo[let, let Int, let String](Bar.new, 4, "hello")
-- good
Foo[var, Int, ref String](Bar.new, 4, "hello")
-- error: `ref String` does not satisfy constraint `var`
```

Also, Option 3 needs to answer the question: how do we indicate only a single binding parameter?  Ie, a type parameter restricted to a single concrete type, but any binding?

Something like `struct Foo[T: =String]` where the `=` differentiates between a trait called `String`.  

Even if we don't make special syntax for this, workarounds shouldn't be that painful (eg making a trait with the same name, or just implementing the type multiple times for each desired concrete binding).  So for now, to avoid the added complexity of generic parameters, we're just gonna stick with generic types (each of which includes a generic binding).


Enough about syntax.  What about lifetime analysis?

### Lifetime Analysis: impact on borrow checking
Any type that includes a borrowed field (let/mut) must not escape the lifetime of that field.  My hope is that this is possible to do in the same way that we track lifetimes of other borrows in scope.  In other words, track each borrowed field as if it is a borrowed variable in scope, and when the container is moved, ensure each borrowed field does not escape.

Of course the same conservative rules must apply.  For example, when a value containing borrowed fields is returned from a function, but the compiler cannot determine the exact provenance of a particular borrow (eg due to runtime conditions) then the field(s) is/are assumed to have the shortest lifetime.