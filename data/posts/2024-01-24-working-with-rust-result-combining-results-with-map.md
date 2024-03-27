---
title: Working With Rust Result - Combining Results with Map
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---

What if we only wanted to parse two numbers and add them together and not return any errors? We can already solve this with `and_then` as before:

```{.rust .scrollx}
parse_number("10")
  .and_then(|ten| {
      // We have successfully parsed "10" into 10.
      parse_number("20")
        .and_then(|twenty| {
            // We have successfully parsed "20" into 20.
            Ok(ten + twenty)
        })
  })
```

We could also just `map` over the last function that returns a `Result`:

```{.rust .scrollx}
parse_number("10")
  .and_then(|ten| {
      // We have successfully parsed "10" into 10.
      parse_number("20")
        .map(|twenty| { // We map here
            // We have successfully parsed "20" into 20.
            ten + twenty // We didn't have to wrap the answer in a Result, because we are 'in' a Result
        })
  })
```

Reminder about `map`s definition:

```{.rust .scrollx}
pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> Result<U, E> {
    match self {
        Ok(t) => Ok(op(t)),
        Err(e) => Err(e),
    }
}
```

in summary:

```{.rust .scrollx}
// pseudocode for map
// Given a Result<T, E>

F: T -> U // Convert success value to a U

Ok(t:T)   ->  F(t)  -> U // Return converted value in Ok, as a Result<U, E>
Err(e:E)            -> E // Return existing error as Result<U, E>
```

We can see that after we apply the function `F`, we still return a `Result`. This is why we can use `map` within an `and_then` call.

So when do we use `and_then` when chaining `Result`s and when do we use `map`?

> If you need to make a decision about whether to fail or not, then use `and_then` because you
can return an `Ok` to succeed or an `Err` to fail. If you simply want to work on a the `Ok` side of a previous `Result`, then use `map`.


Continue on to [The Question Mark Operator](2024-01-24-working-with-rust-result-combining-results-question-mark-operator.html)
