# mismo
Mismo is an exploration of how mutable value semantics can be made safe, powerful, and ergonomic.  

Mismo intentionally does **not** support:
- Garbage Collection 
- methods
- inheritance / subtyping
- traits / type-classes
- constraints on generic type parameters 
- first class references (storing references in structs, returning references from functions)

Mismo **does** feature: 
- **Readable Syntax** with UFCS and UCS; inspired by Python, [Inko](https://inko-lang.org/), and [Ante](https://antelang.org/).
- **Memory Safety** thru affine types, mode system (second-class references)
- **Type Safety** thru statically checked nominal algebraic data types (and structural types via tuples)
- **Ergonomic Generic Programming** thru function overloading and semi-implicit dictionary-passing

Learn more about the Mismo philosophy and distinctives at the [[Mismo Language Tour]].

Or take the deep dive and read the (outdated) [[Mismo]].

## Project Status

Mismo is currently pre-0.1.0 and both the design and implementation are in active development.  It currently transpiles to Zig, but perhaps one day will target LLVM.

## Installation

TODO: Write installation instructions here

## Usage

TODO: Write usage instructions here

## Development

TODO: Write development instructions here

## Contributing

1. Fork it (<https://github.com/scwunch/mismo/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [Ryan Martens](https://github.com/your-github-user) - creator and maintainer
