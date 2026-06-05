---
created: 2026-01-15 21:51
tags:
  - programming
related:
  - "[[Mismo]]"
---
In Mismo, only one of the four [[26Ja5 Memory Safety in Mismo#The Mode System|modes]] is first class, `var`; the other three are *projections* (ie, borrowed references: `mut!`, `mut`, `let`).  Projections are generally treated as second class in Mismo, meaning they cannot be be stored in structs, containers, or be returned from functions.

This is a core feature of Mismo's memory safety model, but it is quite restrictive.  One thing I want to do is implement a limited form of lifetime analysis to allow projections to re-project parameters back to the caller.  That is, allow functions to return references as long as the compiler can prove the reference is borrowed from one of the functions parameters (and therefore lifetime is live in the caller).  This should be a minor extension to the provenance tracker we're already planning on implementing.

This however only goes a short way in solving the ergonomics problem of second class references.  The bigger opportunity here is *storing* references.  This is useful for implementing graph-like data structures, wrappers for references (like `Option[let T]`), external iterators, and other things.

I've considered many potential options to fill this gap including:
- allow any field to be a reference, and then demote the mode of the container to the mode of the reference
	- *as it turns out, this one is actually unsound*
- make "special" monads specifically for `Option[T]` and `Result[T, E]` where `T` is a reference 
- allow tuples (and not other structs) to contain references, as tuples are more "transparent" to the compiler 
- implement a separate universe of types, "view types" that can contain references, but are restricted from appearing as type parameters of regular types, and subject to lifetime analysis.
- only allow closures and coroutines as the exception for structures that can store references 

I found these ideas ultimately unsatisfactory because each one was either lacking in expressive power and/or converging on lifetime parameters, which was an explicit non-goal.

# Our Options
There are currently two ideas that still hold promise.
1. Reference fields coupled with generic modes and lifetime analysis 
2. Add first class reference modes that are garbage collected or reference counted
## Option 0: No Language Support
*Smart Pointer Wrapper Types*

It should be noted at this point that we haven't thrown out option 0 at this point, which is to not add any other features to the language but add first class references in the form of wrapper types like `Rc`, `RefCell`, `Arc`, etc.  This is a good default option if we don't find anything better.  But also, it will likely *still* be a good option even we provide language-level support for something else.

## Option 1: Reference Fields
The idea here is to make the *mode* part of the *type*.  Then allow a field of a struct or enum to be any *type* (including second-class modes) and likewise type parameters may be filled with any type-mode combination.

The implications of this are pretty huge for both compiler complexity and mental-model complexity.  The compiler now needs to track lifetimes and/or provenance of all *fields* (not just variables) to determine when a value is allowed to be moved or returned from a function.  The user now needs to write generic code while keeping in mind all potential modes.

This would necessitate at the very least a syntax to restrict the mode of a type parameters.  For example, `T: Stringable`  might be any type that can be converted to a string, but `mut T: Stringable` might be any type like that but is also required to be *mutable*.  We would also consider syntax like `mut+ T` or `T: #mut` etc.  

To be honest, I'm not sure how much pain this would actually be in practice (I've never written Rust code with or without lifetime parameters).  I'm also not sure at one point the user will start feeling the need for lifetime annotations.

It is complex, but this is likely the most thoroughly expressive design (that still maintains memory safety).

## Option 2: First-class Reference Modes
Another possibility is to add these three modes:
- `ref`: immutable
- `box`: mutable as `mut`
- `box!`: mutable as `mut!`

Each of these modes is both first class and *shareable*, ie, can be aliased.  Under the hood these could be implemented as heap-allocated pointers and garbage collection or reference counting.  

Any field or type parameter could then either be `var`, or any of these first class modes as instead.  To maintain memory safety, once a value is *moved into* one of these modes, it cannot be taken out again.  However, the value can still be projected as one or more of the projection modes.

Our subtype hierarchy gets more complicated now.  In addition to:
```
var <: mut! <: mut <: let
```

We now also have:
```
var <: ref <: let
var <: box <: mut
var <: box! <: mut!
```

And all transitive subtyping relations still hold.

When accessing or *projecting* a non-`var` field, what mode should that projection be?  It depends on the mode of the parent value.  Columns are field modes.  Rows are parent value modes.

| .     | var  | ref | box | box! |
| ----- | ---- | --- | --- | ---- |
| var   | mut! | ref | box | box! |
| box!  | mut! | ref | box | box! |
| box   | mut  | ref | box | box  |
| ref   | let  | ref | ref | ref  |
| mut!  | mut! | ref | box | box! |
| mut   | mut  | ref | box | mut  |
| let   | let  | ref | let | let  |

Things to note:
- a `ref` field can *always* be accessed as `ref`
- a `box` or `box!` field can only be accessed as a first class mode if the permission of the parent's access mode allows it

### Consistency Problem
One issue with this system: when you put a higher-permission mode inside of a lower permission one (for example a `ref` type with a `box!` field), it undermines the guarantees of that mode.  For example, that `ref` may now actually be modified if its `box!` field is aliased before the `ref` is created.

Specifically, the problematic mode combinations are: `ref.box`, `ref.box!`, and `box.box!`.  

Regarding the last combination, an argument may be made to simply allow it to exist, as it does not impose a memory safety problem any more than aliased `mut` and `mut!` references do, as long as the provenance tracker registers the existence of the `box!` in the middle of an access path when a projection is obtained.  That is, the provenance tracker must pay attention to `let x = foo.bar.get(0)` even if `foo` is `box` (or `mut` for that matter) as long as `bar` is known to be `box!`.  Alternatively, `box!` could be projected as `mut` but still potentially be aliased as a `box!` elsewhere.  This allows slightly more consistency, and it's simpler for the provenance tracker.  The opposite argument would be for a consistent mental model where `box` simply *certainly* never allows dynamic mutation.  I'm inclined toward consistency and guarantees.

But regardless of where we land on `box!`, `ref` is a bigger issue.  Now, including a `box` or `box!` in a `ref` is not just a matter of bypassing immutability guarantees (which in itself is worse than just *upgrading* mutation abilities), but it's also an issue of thread safety.  `ref` is the perfect candidate for immutably sharing data across threads.  But if there's a backdoor for mutability... just let's not do that.

#### Field Mode Conversion 
So how do we solve this problem?

Well, I could simply forbid the inclusion of higher-permission shared modes in lower-permission values.  This would be a compile-time error emitted upon *instantiation* of such a type.  Or, we could do one better: ***field mode conversion***

For type `T` bound as mode `a` and containing a field of mode `b`, if `b` has higher mutation capabilities than `a`, then `b` is treated as if it is in fact `a`.

For example given type Foo
```
struct Foo
	var  a A
	box! b B
	box  c C
	ref  d D
```

If `Foo` is bound as a `var` or `box!`, then those fields retain their modes as written.

If `Foo` is a `box Foo`, then field "b" will be converted from `box!` to `box` as well.  That means when a `box Foo` is constructed with `Foo(a, b, c, d)` then expression `b` must be `box` or `var` (in which case it is first implicitly converted to `box`).

If `Foo` is `ref Foo` then fields "b" and "c" are both converted to `ref`, and construction of `ref Foo` is likewise limited to `var`s and `ref`s.

A `var Foo` cannot be converted to `ref Foo`, or `box Foo`  due to these limitations; if the value has already been constructed, the compiler has no way to guarantee the fields are not aliased.

If `Foo` did *not* have a `box!` field, then `var Foo` could be moved into `box Foo` (but not `ref Foo`).  Furthermore, if `Foo` also did not have a `box` field (ie, all fields are `var` and `ref`) then `var Foo` could be moved into any mode.

### Implications for Provenance Tracker
The provenance tracker now also has to track `box!` as potential members of a provenance set since it can be cast as `mut!`.  In practice I don't think this will be that impactful since basically the only reason for a function to take a `box!` parameter is specifically to store it (share it) in some other struct, and not necessarily to modify it as well.  However, I might make `mut!` more plentiful in general, which will lead to more false positives by the provenance checker.

The provenance tracker does not need to track the provenance of the first-class modes because they cannot be invalidated.  Even if an array reallocates a buffer or "pops" the reference, the garbage collector or reference counting mechanism endures the value on the heap remains valid as long as references still exist.

### The Trade Off
This does give us a *lot* of modes (seven in total now, beating Pony's six).  The programmer now also needs to somehow memorize the field projection chart above.  It's definitely complex, but it's the kind of complexity that does not "infect" simpler code in Mismo.  Unlike reference fields do.  In particular, I never need to ask, "What if T is instantiated by a non-first-class type?"

## Option 0: Wrapper Types
If should be noted option 0 (smart pointer wrapper types) may still suffer from the consistency problem noted above.  Eg, if we have a `Ref[T]` type, and then `T` has a field of type `Box[U]`, then how does `Ref` truly ensure that that field is not aliased and thereby sneakily mutable?

Even if you manage to control access to these fields, I don't see any good way of preventing a `Box` from being aliased *before* it's used to construct a `Ref`.

So I don't think it's feasible to go with this structure.  But we could still probably do something like `Arc<T>` which does not by itself necessarily guarantee deep immutability or thread safety.  It depends on the `T`.


## Which Option is Best?
Shall we stick with option 0, or try (1) [[#Reference Fields]], or (2) [[#First-class Reference Modes]]... or maybe even a combination of all three?  That's possible, as these options are not mutually exclusive, but also definitely overkill.

I kinda like the purity of the model where access modes are just for variables and parameters, and do not apply to fields and therefore are *not* part of the type.  

The advantage of including ref/box modes is it gives us a truly immutable mode... but it comes at the expense of increased complexity of the mode system, and makes modes part of the type.

I think I'm liking the idea of wrapper types more and more as I think about it.  And thread safety might not be too complicated either: all values are thread safe by default (as long as it is a `var`, unless it contains a non-thread-safe value, or is marked explicitly as not thread safe.  Which would be the case for a `Box[T]` type were we to include one.  Which means we could still make `Gc[T]` (ie, a garbage collected pointer) to be threadsafe *and shareable* as long as it remains immutable and `T` is threadsafe.

### Conclusion 
Just wrapper types for now.  The advantages of `ref`, `box`, and `box!` are not worth the added complexity.  But keep the future possibility of supporting first-class reference modes in a limited or full way in the future.

## Other Ideas
### Coroutines + InOut
Mismo's design decision to support only second class references precludes storing references in structs.  This puts us in a difficult position when we think about accessing elements of an array.  If we truly stick to second class references, then we cannot return them from functions, and therefore no function could give us a reference to an element.  

But let's say we allow the compiler to do some escape analysis.  Then we might be able to write functions that can return references to their parameters, and therefore write a "get" function for `Array`, but even then, we cannot wrap it in an `Option` because that would involve storing the reference, which is not possible.  That's not ideal because then we have little choice but to panic on out-of-bounds access.

We could do this if we first cloned every element we access.  But this of course requires the element type to implement Clone, and introduces some overhead, and is not ergonomic for mutating elements.

The other possibility is to write a "get" function that takes a closure, and then the get function body calls the closure on the element (if it's not out of bounds).  But there is a subtle catch with this one: again, sticking strictly to second class references, not even closures can store references, so the power of the closure is greatly limited in what it can actually do.  (Users might end up writing a different closure for each branch of logic in the caller's scope since branching inside the closure based on references in the caller is not possible.)

We could allow closures to be an exception to the "no references in structs" rule, but that adds much compiler complexity, and encourages some programming patterns that I'm not sure I want to encourage.  And if we introduce that level of complexity, then at that point we might as well just support first-class references everywhere.  So that's a conversation for another time.

### Another type universe
- allow structural types (ie, tuples and unions) to contain references
- and these types are subject to lifetime analysis
- types in this universe would be restricted from appearing as type parameters of regular types
- ... this of course requires implementing a parallel type system and ensuring it integrates nicely with the rest of the language.

### The Closure Exception
Allow closures to capture by reference, and then apply lifetime analysis to such closures.  This is actually essentially the same as the separate type universe, but it feels a little more natural.

It also works out quite nicely for iterators, I think, which can be transformed into coroutines (and we would make coroutines the exception too of course).

### Nullable borrows
The primary reason we want to store references is just to support returning `Option[mut T]` and the like.  So what if we just encode that use case as another set of modes: 
- `let?`
- `mut?`
- `mut!?`

Then have a compiler-recognized function (or some other syntax) to transform these into regular borrow modes.
```
fn get(mut self Array[T], index UInt) mut? T: 
	...
	
mut array = make_array()
mut? x = array.get(i)
if x:
	-- ... do something with x as mut T
```

This does not help us with iterators storing references, or `Result[mut T, E]` or non-capturing closures.

### Ephemeral tuples and unions
or "Multiple Return Values & Multiple Return Types"
```
-- union return
fn get(mut self Array[T], index UInt) -> (mut T | Nil): 
	...
	
mut array = make_array()
-- must perform match immediately
if array.get(i) is
	T(t): 
		print("got element: \[t]")
	Nil(_): 
		print("index out of range")
		

-- tuple return
fn iter(mut self Array[T]) -> (ArrayIterator[T], mut Array[T])
```