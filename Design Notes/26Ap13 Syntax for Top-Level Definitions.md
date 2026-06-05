---
created: 2026-04-13 14:49
---
Previously, Mismo used this syntax: 
```
struct Foo
	var field: Field
	
	constructor(arg: Type): 
		body
	
	static fn new(arg: Type) -> Foo: 
		Foo(arg)
		
	fn pseudo_method(arg: Type) -> Return:
		body 
		
	fn mut change_me(): 
		body
```

Note: 
- four different types of items, all nested under `Foo`
- four different keywords: `var` (for field), `constructor`, `static fn`, and `fn` (for pseudo-method)
- methods have implicit `self` parameter
- parameter mode for `self` must be put before function name

Now I'm thinking this should be translated to: 
```
struct Foo
	field: Field
	
	constructor(arg: Type):
		body

fn Foo.new(arg: Type) -> Foo: 
		Foo(arg)
		
impl Foo
	fn pseudo_method(self, arg: Type) -> Foo:
		body
		
	fn change_me(mut self, arg: Type):
		body
```

Note:
- custom constructor is linked to type definition
- static functions must be defined at top-level
- `impl` blocks can go anywhere
- ... require explicit `self`?

*What about custom constructors for enums*?  
```
enum IpAddr
	V4(Int, Int, Int, Int)
		constructor(a: Int, b: Int, c: Int, d: Int):
			if 255 <
				a: panic
				b: panic
				c: panic
				d: panic
			IpAddr.V4 {a, b, c, d}
			
	V6(Int, Int, Int, Int, Int, Int)
```

```
enum IpAddr
	V4(V4)
	V6(V6)
	
struct V4
```

Nah maybe later.

### And what about traits?
I think I've narrowed it down to two choices: "interface" or "capability".

Below are a bunch more options.
```
constraint Quibble[A, B]
  fn quibble(a: A, b: B)

fn perform_quibble[T](a: Int, b: T) where Quibble[Int, T]: 
  quibble(a, b)
``` 

```
protocol Quibble[A, B]
  fn quibble(a: A, b: B)

fn perform_quibble[T](a: Int, b: T) where Quibble[Int, T]: 
  quibble(a, b)
```

```
contract Quibble[A, B]
  fn quibble(a: A, b: B)

fn perform_quibble[T](a: Int, b: T) where Quibble[Int, T]: 
  quibble(a, b)
```

```
capability Quibble[A, B]
  fn quibble(a: A, b: B)

fn perform_quibble[T](a: Int, b: T) where Quibble[Int, T]: 
  quibble(a, b)
```

```
capability Optional[T]
  fn unwrap(self: Option[T]) -> T
  
fn foo(opt: Option[Int]) where Optional[Int]:
						 -- fn unwrap(self: Option[Int]) -> Int
	opt.unwrap
	
	
impl[T] Option[T] where Stringable[T]
	fn string(self) -> String:
		if self is 
			Some(t): t.string
			None: "none"
```