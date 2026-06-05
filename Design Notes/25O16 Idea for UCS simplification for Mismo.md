---
created: 2025-10-16 22:34
related:
  - "[[Mismo]]"
---
#2025/Oct/16 #programming 
***
Inspired by Gleam's design decision to commit to pure pattern matching, no traditional if statement.  So what in other languages would be 
```
if condition {
    "Yes"
} else {
    "No"
}
```
In gleam would be
```
case condition {
    True -> "Yes"
    False -> "No"
}
```

Mismo's UCS at present can do both.  But here's the idea, phrased as a question:

Would the design be cleaner if we, like Gleam, disallow the traditional usage of `if`?

Assuming we do go that route, would having a syntax sugar for matching booleans be worth it?

To be clear, this means the first statement above would be written I Mismo like this:
```
if condition ==
    true: "Yes"
    false: "No
```

To be honest, that might be even better than 
```
if condition:
    "Yes"
else:
    "No
```

Well, pros and cons anyway.

Then what if we add in a bit of syntax sugar:
`if condition: "Yes"` => `if condition == true: "Yes"`

Oof, that's not worth it.

What about:
`"Yes" if condition else "No"` => `if condition==true: "Yes" else: "No"`

Maybe.  

But anyways, what's the purity/cleanliness we're gaining by foregoing the  usual if statement?

I think the gain is in the concept of the "else" keyword.  The "else" keyword is strictly a fallback (always true) branch, rather than the alternative to a single boolean condition.  Oh wait, that is actually kinda the same.

Okay, so I guess the real gain is that each test of the conditional is consistently of the form `<term> <op> <term>`.  But I dunno, that's not really a huge win for purity, because the mental burden of adding another form, `<cond>`, is hardly a burden at all.  In fact, it would probably be a burden to remove.

So IN CONCLUSION:
This simplification has no advantage unless we can find a compelling syntax sugar to replace what has been lost, but I can't think of anything better than the simply expected original.