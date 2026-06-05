---
created: 2026-03-20 09:50
related:
  - "[[Mismo]]"
tags:
  - programming
---
```crystal
fn main:
	var array = [1, 2, 3, 4]
	co iter = array.iter
	iter.next n:
		print(n)
	else:
		print("empty iter")
	# ^ is syntax sugar for this >
	if iter.next(|n|: 
		print(n)
	).or_else(||:
		print("empty iter")
	)
	
	array.get(5) n:
		print(n)
	else:
		print("index out of bounds")
	# ^ is syntax sugar for
	array.get(5, |n|:
		print(n)
	).or_else(||: 
		print("index out of bounds")
	)
	
```

In general, a function with a signature whose final parameter is a function type that returns an `Option` may be called with this syntax sugar.