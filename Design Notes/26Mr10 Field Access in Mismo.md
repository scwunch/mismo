---
created: 2026-03-10 10:46
related:
  - "[[Mismo]]"
tags:
  - programming
---
This is a design process document about the syntax and semantics of field access expressions, default modes for variables and fields, and how to specify function overloads when a function is overloaded by parameter passing mode.

[Conversation with Gemini](https://gemini.google.com/share/6b43d3c6f94d)

Note that, due to UFCS, the expression `bar.foo` ought to be equivalent to `bar(foo)`.  Unless we overload the semantics of the dot (`.`) operator.

### Default mode of a variable
First design question: what should be the default mode of the atomic expression `foo`, consisting of only of an identifier, declared with `var`, and of a linear type?  Our choice here has far-reaching implications for overload resolution, syntax, and ergonomics of field access.  We basically have three choices: 
1. default to the most powerful mode (ie, `var`)
2. implicitly adapt to the context (ie, the mode of whatever parameter it's being passed to)
3. default to the highest powerful borrow (ie, `mut!`)
4. default to the lowest mode (ie, `let`)

Let's explore the implications of each.

#### 1. Default `var`
This option has the highest affinity with linear logic, since it means that each linear variable can literally only be used once, then it is consumed.  Even the expression `foo.bar` (assuming it's equivalent to `bar(foo)`) should consume `foo`.  (Notably, Rust sets a precedent for this.)

This feels good from a purity and elegance perspective, but in practice we need a way to *borrow* linear values as well.  And we need a way to borrow fields of linear values.  In Rust, this is done with the `&` sigil (or `&mut`) which binds tighter than the dot, such that `&foo.bar` is equivalent to `(&foo).bar`.  In Mismo we could follow suit, but we will need three different symbols or keywords.  

Let's stick with the keywords for now for maximum clarity, talk about operator precedence first, then explore the possibility for alternate syntax (such as sigils).

##### The Case for High Operator Precedence
`mut foo.bar` is equivalent to `(mut foo).bar` and `let foo.quz(bar)` is equivalent to `quz(let foo, bar)`

Programmers are accustomed to dot operators having the highest precedence, or at least higher than prefix operators (again, note that Rust bucks this trend).  So this is not the most intuitive.  

However, this probably leads to less noisy parentheses as I believe the expression `(mut foo).bar` should be more commonly required than `mut (foo.bar)` and likewise `(let foo).quz(bar)` than `let quz(foo, bar)`.  **Why?**  
- You almost never need to explicitly downcast the mode of the result of a function (if you did, you would probably want to bind the result to a variable first anyway).
- Relatively speaking, you would more often want to explicitly downcast the mode of a `var` variable (before getting the field, or passing it to a function)

##### The Case for Low Operator Precedence
If we were to make the precedence lower than `.`, then we would certainly require parentheses every time we want to borrow a `var` *and* every time we want to borrow a field of a `var`.  Ie, we would have to write `(let foo).quz(bar)` or `quz(let foo, bar)` to call `bar` on `foo` without consuming it, and we would even have to write `(let foo).bar` simply to access the `bar` field without consuming `foo`.  

That verbosity is not acceptable to me, so we would need a dedicated syntax for field projection.  That is, we need both the `let` prefix-keyword (for downcasting variables and other expressions) as well as something like `&`/`@`/`!`  for field projections.  This is redundant, therefore *high* operator precedence for mode-casting is preferable.

##### Syntax for mode casting
Note this syntax is only necessary for `var`s.

A set of prefix sigils could make this more clear: 
- `&` -> `let`
- `@` -> `mut`
- `!` -> `mut!`

So this would make `foo.bar` consume `foo` and return a `var` value of the field.  While `&foo.bar` first takes the `let` projection of `foo` without consuming it, and therefore the `let` projection of the `bar`.  

Alternatively, we could make it a postfix operator, and that case we may even consider dropping the dot for field access.  (If we opt for *low* operator precedence, dropping the dot would be all but necessary.)
- `foo&.bar` or `foo&bar` => `let` projection of the field "bar"
- `foo@.bar` or `foo@bar` => `mut` projection of the field "bar"
- `foo!.bar` or `foo!bar` => `mut!` projection of the field "bar"

Finally, we could do postfix with a keyword such as `foo@let` or `foo:let` as in `foo@let.bar` or `foo:let.bar`.  The colon looks nice, but would present some parsing difficulties.

#### 2. Implicit Adaptation of Mode to Context
This still has pretty high affinity with linear logic, and it has the advantage of reducing verbosity.  If linear variables default to `var`, then we always need to annotate the passing mode, even when the only passing mode accepted is `let`.  If we allow this to be implicit, it's intuitive, more ergonomic, and less clutter at each callsite.

However, we still need to annotate the mode of field access, since we really want to be able to infer the type and mode of the expression `foo.bar` and `bar(foo)` without having to look at where that expression is used.  That would mean `foo` is fine to use in functions that require `let`, but `foo.bar` may still consume `foo` and the "bar" field even in contexts where only `let` is allowed.  So we would need to use some of the syntax we suggested above (eg `(let foo).bar` or `foo@bar`) still.

Alternatively, we allow field-access to only borrow (default `mut!`) and introduce another syntax for field extraction (eg, tuple unpacking, or something like `var Foo{bar, ...} = foo`).  Or perhaps something like `foo->bar` to sink `foo` and extract "bar".

Another alternative is to accept `foo.bar` may actually have different semantics, and when it represents field access, it also may adapt to the proper mode.  In fact, this is likely better than restricting field access to return `mut!` as that's not really a valid function return type anyway.

#### 3. Default to Highest Borrow
In practice, this is just like the above, except we require additional syntax (eg a `sink` marker) to consume variables, not just fields.  

This would mean that, for our `var foo`, in the expression `bar(foo)`, `foo` would present itself as a `mut!` and would implicitly downcast to the highest mode overload of `bar`.  If `bar` requires a `var`, then the compiler would prompt you to rewrite it like this: `bar(sink foo)`.  The same would be true for `foo.bar`; it would not consume `foo`, but rather present a `mut!` projection of the "bar" field, or call the "bar" function.  And if `bar` requires `var`, then you would have to rewrite it as `(sink foo).bar` or maybe it would be permitted to write `sink foo.bar`.  I guess the latter would be fine since `sink <identifier>` is the only valid grammar for the "sink" keyword, and that gives us the flexibility later on to implement *partial move* semantics using the same syntax for moving fields if we want.

#### 4. Default to Lowest Borrow
This option is in some sense the safest option.  It means every variable presents as `let` by default, and needs additional syntax for mutation or moving.  It mirrors the "`let` is default" behaviour of parameters, which is a win for consistency.  It also means that implicitly downcasting modes would happen much less often, as you need to explicitly mark every usage of a variable for mutability or sinking.  This is a win for clarity, as both mutability *and* linear value movement are always visible.  

Field access, in this case, would need to still default to the highest mode available, as that mode would be controlled by how the variable itself is projected.  So, both `foo` and `foo.bar` would always be `let` unless specified otherwise.  And if `quz` has one overload, and that overload requires a `mut Foo`, then `quz(foo)` will not cut it, it must be `quz(mut foo)`.  


### Summary
So with all these thoughts out of the way, it looks like we have eliminated some of the solution space and are left dealing with just a few trade-offs.  

- Option 1: `var` variables are *always* consumed (even for field access) *unless* an explicit mode-marker is given (syntax options are not great, but viable)
- Option 2/3: `var` variables, and also field-accesses, implicitly adapt to the context
	- possibly requiring an explicit `sink` for `var`, otherwise defaulting to highest mode available
	- only require mode annotation when disambiguating a function call and choosing a lower passing mode
- Option 4: always default to `let` mode, and require annotations for all other modes

*** 
## Gemini Thinking Response to this Document

> [!thinking]-
> **Considering Mismo Design**
> 
> I'm currently focused on Mismo's design, specifically field access syntax, semantics, and function overload resolution. I'm exploring a 4-mode system (var (owned/sink), mut! (shared mutable/shape), ref (shared immutable/copy), and mut (unique mutable/move)) and how it affects things.
> 
> **Refining Access Modes**
> 
> I'm now refining the four options for default `var` modes, particularly comparing Option 3 (default `mut!`) and Option 4 (default `let`). I'm finding the "greedy" nature of Option 3, triggering provenance, contrasts with Option 4's explicit clarity. I also see implicit adaptation not as flaky, but as a potential tool.
> 
> **Evaluating Access Syntax**
> 
> I'm now weighing the merits of different default variable behaviors within Mismo, particularly for field access. I'm focusing on simplicity versus safety, and seeing that Option 1's linearity is too restrictive, while Option 2 might be too implicit. Option 3 is a middle ground, but I'm leaning toward Option 4, with its explicit `let`. I think its verbosity is acceptable.
> 
> **Analyzing Field Access**
> 
> I'm now analyzing the implications of unifying field access and function calls in Mismo through UFCS, viewing `foo.bar` exactly as `bar(foo)`. I'm exploring whether there's a practical difference between accessing a field versus calling a method, particularly in light of Mismo's design. The persona of an empathetic AI peer is informing my analysis. I'm focusing on syntax options to match Mismo's goals, and leaning towards Option 3 (default `mut!`) with a `sink` keyword to support shared mutability. I'm seeing that defaulting to `let` (Option 4) makes our goal of shared mutability more cumbersome.
> 
> **Clarifying Variable Access**
> 
> I'm now clarifying how `let foo.bar` is equivalent to `bar(let foo)`, treating mode casting as source-based. I'm exploring the need for `sink foo.bar` for moves and `let foo.bar` for downcasting to avoid provenance issues. I'm also ensuring I'll cite relevant papers for context. I think I'm seeing that Option 1 is really a "Sinking" default. I can see how Option 3 (default `mut!`) necessitates explicit syntax for different access modes.
> 
> That is a stellar breakdown of the design space. You’ve successfully identified the "Uncanny Valley" of field access: if we treat it as a pure function call to maintain UFCS symmetry, we lose the "magic" that most languages use to make dot-access ergonomic (like Rust’s autoref/autoderef).
> 
> Here is my analysis of your options, filtered through the lens of Mismo’s stated goals of balancing safety with shared-mutability "superpowers".

---

### 1. The Critique of Option 1 (Default `var`)

While this is the most "pure" for a linear language, it is arguably the most hostile to the developer.

- **The "One-and-Done" Problem:** If `foo.bar` consumes `foo`, you effectively cannot have a struct with two fields that you want to use in the same scope without immediate, heavy use of mode-casting.
    
- **The Rust Comparison:** It’s worth noting that while Rust moves by default, the dot operator is its one major exception—it automatically borrows. If Mismo rejects "special dot magic" to preserve UFCS, Option 1 makes the language feel like you're fighting for your life just to read a `struct` property.
    

### 2. Option 3 vs. Option 4: The Heart of Mismo

This is where the real choice lies. It's a trade-off between **Ergonomics (Opt 3)** and **Auditability (Opt 4)**.

#### Option 3: Default to Highest Borrow (`mut!`)

- **Why it fits:** Mismo views shared mutability as a "dream for Rust developers". Defaulting to `mut!` makes that dream the path of least resistance.
    
- **The "Sink" escape hatch:** Requiring `sink foo.bar` to actually move a field is a very clear signal. It keeps the destructive actions visible while letting the "normal" work (reading/modifying) stay clean.
    
- **The Provenance Cost:** The main downside is that the compiler is always "on edge," tracking provenance for almost every expression because it assumes you _might_ perform a shape-unstable mutation.
    

#### Option 4: Default to Lowest Borrow (`let`)

- **Why it fits:** It is perfectly consistent with Mismo's "default to immutability" philosophy for parameters.
    
- **The "Stutter" Problem:** You would end up writing `mut` or `!` everywhere. `mut person.name = "Ralph"` or `p.move_to(mut destination)`.
    
- **The Auditability Win:** This is the "Grounded AI" choice. You can look at any line of code and know exactly where mutation is possible without looking at function signatures.
    

---

### The "High Precedence" `let` is a Winner

Your realization that `let foo.bar` should bind as `(let foo).bar` is a significant ergonomic breakthrough. It solves the "noisy parentheses" problem that plagues languages with explicit referencing. It allows the mode-cast to act as a **modifier on the path** rather than a wrapper on the result.

### My Recommendation: Option 3 with a "Stingy" Twist

I lean toward **Option 3 (Default `mut!`)**, but with one specific guardrail to address your concerns about "flaky" context-dependence:

1. **Identifier Mode:** An atomic identifier `foo` defaults to `mut!`.
    
2. **Overload Resolution:** If `bar(foo)` is called, and `bar` has overloads for both `let` and `mut!`, Mismo should **prefer `let`** if no mutation occurs in the function, or simply **default to the lowest mode that satisfies the call.**
    
3. **The "Sink" Requirement:** You must use `sink foo` to move it.
    

This gives you "Rust-like" safety (you can't accidentally move something) but "Python-like" ergonomics (you don't have to put `&` or `let` on every single property access).

---

### One potential edge case I missed:

What happens with **chained access**? In `company.owner.name`, does the `mut!` permission propagate all the way down? If `company` is borrowed as `mut!`, does that mean `owner` and `name` are also automatically `mut!`?

Under the UFCS rule, `name(owner(company))`, the answer is yes—the mode of the "inner" call determines the maximum available mode for the "outer" call.

**Would you like me to draft a few code examples showing how `sink`, `mut!`, and `let` would look in a complex nested data structure under Option 3?**