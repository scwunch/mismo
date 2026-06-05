---
created: 2025-09-15 15:36
related:
  - "[[Mismo]]"
---
#2025/Sep/15 #programming  
***

In the current version of [[Mismo]], it is acknowledged that we should have a way to return references (ie, a function signature that ends with `-> let String` or `-> Option[mut T]`).

The original idea for supporting this involved very simple analysis: if the compiler can prove that a reference descended from one of the `let`/`mut`/`inout` parameters, then it can be returned.  Otherwise raise an error.  This is fine, but limited.  For instance, when references of differing lifetimes are passed to a function, then the inferred lifetime of the returned reference is flattened to the lowest common denominator.  At least without further analysis.  Also, it still doesn't allow us to wrap those references in, eg, an `Option` type, which is a pattern I wanted for indexing containers.

So there were a few non mutually exclusive ideas for expanding the power of this:
 - allow the storage of `let`/`mut` references in structs, with the restriction that the binding of the container must be either `let` or `mut`. 
 - lifetime annontations lite™ — allow some parameters to be marked:
	 - as `iso` (meaning the compiler must guarantee non-overlapping references); 
	 - and/or marked as `yield` (or something similar) to allow only such references to appear in return value

This has yet to be investigated thoroughly in terms of ergonomics, memory safety and complexity (for both programmer and compiler).  But it's clear that this does involve significant complexity, no matter how you slice it.

### Return References Via Macro
Another potential solution that bypasses some of these difficulties is to completely disallow return references from *functions*, but make *ergonomic macros* instead for this purpose.  Since macros are kinda like *inlined functions*, we could just leverage whatever semantic analysis already exists for managing reference lifetimes within a function body.

Very basic example:
```
macro max(a, b):
	if a >= b:
		a
	else:
		b
		
fn main:
	mut one = 1
	mut two = 2
	mut bigger = max(one, two)
	-- expands to: mut bigger = if (one) >= (two): (one) else: (two)
	bigger += 2
	print(two) --> 4
```

array indexing:
```
macro get(list, index):
	mut list = $list
	let idx: UInt = $index
	if idx < list.count:
		list._buffer.get(idx)
	else:
		panic("index out of range")
```

Okay, as I write these, it's becoming more obvious that full-fledged macros introduce some unwanted complexity into writing this kind of function.  I guess what we actually want is something more akin to Hylo's *subscripts*.  

### Return References via Inlined Function
What about just a simple `inline` keyword?  One keyword to trade the power of recursion for the power to return references?

```
inline fn max(mut a Int, mut b Int) -> mut Int:
	if a >= b:
		a
	else:
		b
		
inline fn get(mut list: Array[T], index: UInt) -> mut T:
	if index < list.count:
		list._buffer.get(index)
	else:
		panic("index out of range")
		
fn main:
	var array = [1, 2, 3, 4]
	mut second = array.get(1)
	-- expands to:
	mut second = (
		mut list: Array[UInt] = array
		let index: UInt = 1
		if index < list.count:
			list._buffer.get(index)
		else:
			panic("index out of range")
	)
	--
	second += 100
	print(array) --> [1, 102, 3, 4]
```

I guess this works... it just requires a second type of function that must be inlined... which means that it can't be recursively called (even indirectly).  That would likely be rare, but also would likely be a confusing error message.  Speaking of error messages, what would a lifetime error look like here?

```
fn main:
	var array = [1, 2, 3, 4]
	mut second = array.get(1)
	mutating_function(array)
	print(second)
	-- ERROR: Cannot use `second` here, it might have been dropped in the previous line
```

Okay I guess the error message there is fine.  What about when it occurs in the body of the inlined function?
```
inline fn get(mut list: Array[T], index: UInt) -> mut T:
	mut el = list._buffer.get(index)
	modify(list)
	el
	-- ERROR: cannot use `el` here as it may have been dropped
```

Okay, but hold on, what have I actually gained by inlining?  This is just lifetime analysis all over again.

```
inline fn get_reference(mut array: Array[T], mut red_herring: Array[T]) -> mut T:
	array.get(0)
	
fn get_first(mut array: Array[T]) -> mut T:
	var not_used = [1, 2, 3, 4]
	get_reference(array, not_used) -- this errors unless it's inlined
	
fn main:
	var array = [1, 2, 3, 4]
	mut el = get_first(array)
	
-- expands to:
fn main:
	var array = [1, 2, 3, 4]
	mut el = (
		mut array: Array[UInt] = array
		var not_used = [1, 2, 3, 4]
		(
			mut array: Array[UInt] = array
			mut red_herring: Array[UInt] = not_used
			array.get(0)
		)
	)
	-- lifetime analysis passes!
```

Ah, I guess the benefit is delaying the lifetime analysis to *after* the inlining.  That makes `get_first` actually work in some 

### Return References via `unsafe` code
I think we need to get more creative if we want to avoid lifetime annotations while still allowing patterns like this. What if we just pass off the memory safety to the user in cases like this? We could maintain our original two rules, ie, disallowing references to be stored in structs and disallowing references to be returned from functions... *unless* the programmer opts into unsafe code. So it would be considered a kind of "advanced" programming pattern to return references or store them in structs (eg `Option[mut T]`) and the programmer agrees to take responsibility for lifetimes at that point.

Bit of a cop-out, but at least it maintains ergonomics.  But this of course could easily be overused.

### Return references via a special runtime-checked handle
Something like Swift's keypath, or a built-in version of `Option[mut T]`.  