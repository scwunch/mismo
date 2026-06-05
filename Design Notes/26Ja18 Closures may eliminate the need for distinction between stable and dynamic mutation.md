---
created: 2026-01-18 11:09
related:
  - "[[26Ja5 Memory Safety in Mismo]]"
  - "[[26Ja15 How to store references in Mismo]]"
  - "[[Mismo]]"
tags:
  - programming
---
Thinking about Mismo's mode system and how to store projection modes.

This line of thought began with the observation that if we do not allow borrows to be stored in containers, then the desired function signatures for "get" for containers is not possible.
```
fn get[T](mut! self: Array[T], index: UInt) Option[mut! T]
-- "mut! T" in Option[mut! T] is not a type.
-- you can't store a `mut!` in a struct/enum
```

So if we want to have safely bounded array access, we can change the signature to instead accept a closure:
```
fn get[T, R](
		mut! self: Array[T], 
		index: UInt,
		action: {T->R}
	) Option[R]
```

And the closure would run and return a value only if the index is valid.  This is akin to internal iteration, but just over one value.  `R` could be `Nil`, of course, if you don't need to return anything. You can then call it like this:

```
if my_array.get(5, |elem|: 
	-- do something with `elem`, which is `let`, `mut`, or `mut!`
) is
	Some(res): 
		-- `res` is whatever the closure returned
	None: 
		-- in this scope we know that the index (5) was not valid
```

What if we want *external iteration*?  Well, if we do some lifetime analysis, we may be able to return capturing closures from functions.  Then we could write and use our "get" function like this:

```
fn get(mut! self Array[T], index UInt) Maybe[T]:
   {|do: {T->R}|:
      if index < .size:
         Option.some(do.call(.buffer.get(index))
      else:
         Option.None
   }
   
type Maybe[T] = {
   fn call(do: {T->R}) Option[R]
}

fn main:
   var arr = [1,2,3,4,5]
   var third = arr.get(2)
   third.call(|elem|:
      print(elem)
   ).or_else(||:
	  print("third element not found.")
   )
```

Recall that the `mut`/`mut!` distinction only exists because of the possibility of internal references being invalidated by mutation to dynamic containers.  

While we look at how this example is written, the internal reference looks like it would always be quite short-lived.  This opens up the possibility for another mechanism to ensure memory safety: outlaw simultaneous access to both container and internal reference.

And if we want to dump the complexity of the provenance tracker, we may enforce this by requiring an `inout` mode for the Buffer.get function.

Otherwise, we could keep the provenance tracker, but enforce it with a variation of the mutual exclusion principle: you cannot pass a mutable borrow of a container while also passing a borrow of its contents to one function call.

Actually that might be a fine rule even without the closure craziness.

```
var 
```


***
```
struct ArrayIterator[T]
	.array Array[T]
	.index UInt = 0
	
	fn next[R](mut self, do: (let T)->R) Option[R]:
		var r = .array.at(.index, do)
		if r is Some(_): .index += 1
		r
			
	fn get_array(move self):
		.array
		
fn main:
	{array: var array, ...} = array_iter
	array = array_iter.array
	var array = array_iter.tuple.array
```