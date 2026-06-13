# mismo
Mismo is an exploration of how mutable value semantics can be made safe, powerful, and ergonomic.  

Mismo intentionally does **not** support:
- garbage collection 
- methods
- inheritance / subtyping
- traits / type-classes
- lifetime parameters
- first class references (storing references in structs, returning references from functions)

Mismo **does** feature: 
- **Readable Syntax** with UFCS and UCS; inspired by Python, [Inko](https://inko-lang.org/), and [Ante](https://antelang.org/).
- **Memory Safety** thru affine types, mode system (second-class references)
- **Shared Mutable References**, safely and without runtime checks, thru the mode system and provenance tracking
- **Type Safety** thru statically checked nominal algebraic data types (and structural types via tuples)
- **Ergonomic Generic Programming** thru function overloading, continuation-passing style, and semi-implicit dictionary-passing

```mismo
enum Option[T]
	Some(T)
	None
	
	
fn print[T](val T, ?to_string (T)->String): 
	io.println(val.to_string)


fn to_string[T](opt Option[T], ?to_string (T)->String) -> String: 
	if opt is 
		Some(val): "Some(\{val})"
		None     : "None"

        
fn map[T, U](var opt Option[T], fun (var T)->U) -> Option[U]: 
	if opt is
		Some(val): Some(fun(val))
		None     : None


fn mutate[T, U](mut! opt Option[T], fun (mut! T)->U): 
    if opt is
        Some(val): fun(val)


fn double(mut n Int): n *= 2


fn main(): 
    var x = Option.Some(21)
    x.mutate(double)
    let result = x.map(|n|: n * 10)
    print(result)
```

Learn more about the Mismo philosophy and distinctives at the [Mismo Language Tour](Reference/Mismo%20Language%20Tour.md).

Or take the deep dive and read the (outdated) [Mismo](Design%20Notes/Mismo.md) and other [Design Notes](Design%20Notes).

## Project Status

Mismo is currently pre-0.1.0 and both the design and implementation are in active development.  It currently transpiles to Zig, but perhaps one day will target LLVM.

## Installation & Usage

To compile the Mismo compiler: 
- install Rust and Cargo
- install [Lisette](https://lisette.run/) => `cargo install lisette`
- clone this repo
- `lis run -- [args]` to run the Mismo compiler
- `lis run -- std/prelude.mismo` to compile the prelude
- `lis run -- PATH.mismo` to compile and run a `.mismo` file
- `lis run -- --test [test_names]` to run mismo compiler tests

## Contributing

1. Fork it (<https://github.com/scwunch/mismo/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request
