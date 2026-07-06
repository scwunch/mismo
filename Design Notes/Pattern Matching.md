---
created: 2026-07-07 00:10
related:
  - "[[Mismo]]"
---
[Relevant conversation with Claude](https://claude.ai/share/b1ee388f-ccd3-44f2-9676-8d6547e04ddc).

I was going to try to adopt UCS with some minor changes, however, I find the edge cases to be awkward, especially around how indentation affects operator precedence, and UCS is so flexible, it's actually *too* powerful for my taste. There are so many ways of writing the same thing.

So here's the design I'm considering now.

We start with traditional if-statements: 
```
if condition: 
   do-this
else: 
   do-that
```

Then we add basic pattern matching via a special `is` operator. The left-hand-side of the `is` operator is an expression, but the right-hand-side is a pattern that has the ability to bind names as well as produce a boolean value when used as the condition of an `if` statement.
```
if opt is Some(value): 
   print(value)
```

Furthermore, multiple patterns may be indented under an `is` creating multiple branches of the `if` statement.
```
if res is 
   Ok(value): print(value)
   Err(e): print("oops")
```

The `is` supports pattern matching on enums, tuples (eg `if (a, b) is (Some(a), _):` ), arrays (eg `if arr is [first, ...rest]`), string literals, and number literals.

We also add support for creating patterns out of arbitrary comparison operators, and predicate functions (a pattern starting with `.`).

Example:
```
let limit = get_limit()
if get_number() is 
   < limit:   "less"
   == limit:  "on the edge"
   .is_prime: "big prime"
   _ :        "something else"
-- note if we simply matched on `if get_number() is limit` then it would *bind* the number to the variable `limit` rather than doing equality comparison.
```

Actually, these kinds of patterns are merely syntax sugar for the next feature:
```
if x is 
   < 0:       "negative"
   .is_prime: "prime"
-- is syntax sugar for: 
if x is 
   tmp if tmp < 0:       "negative"
   tmp if is_prime(tmp): "prime"
```

That embedded `if` at the end of the pattern is our support for sophisticated **guard expressions** that are allowed to use any names bound in the pattern they are guarding, *and* may themselves branch into a set of sub-cases.
```
if x is 
   Some(a) if a is
      < 0: "negative"
      0: "zero"
   None: 
      "undefined"
```

If a sub-match expression is not exhaustive, and all the sub-cases fail to match, then execution continues on in the outer match expression.

