---
title: Working With Rust Result - Combining Results with Map - Part 7
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

> `map` wraps the result of `op` in an `Ok` constructor for us so we don't have to!

In summary:

```{.rust .scrollx}
// pseudocode for map
// Given: Result<T, E>
// Return type: Result<U, E>

op: T -> U // Convert success value to a U

Ok(t:T)   ->  op(t) -> U -> Ok(U)  // Return converted value in Ok, as a Result<U, E>
Err(e:E)                 -> Err(e) // Return existing error as Result<U, E>
```

How do we decide when to use `and_then` at the last step of a `Result` chain or whether to use `map`?

> If you need to make a decision about whether to fail or not, then use `and_then` because you
can return an `Ok` to succeed or an `Err` to fail. If you simply want to work on the `Ok` side of a previous `Result`, then use `map`.

> This logic works only at the last step of a `Result` chain. If you use `map` where you should have used `and_then`, you will end up with a nested `Result` of the sort: `Result<Result<T, E>,E>` indicating that you should have `and_then`ed where you had previously `map`ped.

So many rules to keep in mind! If only there were an easier way to combine `Result`s.

Continue on to [The Question Mark Operator](2024-01-24-working-with-rust-result-part-8.html)
