---
title: Working With Rust Result - Tranforming Values - Part 5
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---

When using functions like `map_or_else`, we extracted the success and error values out of a `Result`, thereby losing our `Result` "container". What if you could run a function on the value within a `Result` and stay within the `Result` "container"? Then you wouldn't have to do all this pesky unwrapping until you needed the value.

### map

The `map` function lets you transform a value within a `Result`:

```{.rust .scrollx}
pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> Result<U, E> {
    match self {
        Ok(t) => Ok(op(t)),
        Err(e) => Err(e),
    }
}
```

In the above definition, the supplied function `F` is only run on the value within the `Ok` instance and the error value within the `Err` instance is left untouched.

> After function `F` is used, the result is rewrapped in an `Ok` constructor. In the `Err` case we also rewrap the error again. This might seem pointless, but this has to be done because the result type is changing from a `Result<T, E>` to a `Result<U, E>` and the `Err(e)` in the pattern match is of type `Result<T, E>`. By creating a new `Err` instance we convert the error to type `Result<U, E>`.

```{.rust .scrollx}
// pseudocode
// given a Result<T, E>

F: T -> U // Convert success value to a U

Ok(t:T)   ->  F(t)  -> U // Return converted value in Ok, as a Result<U, E>
Err(e:E)            -> E // Return existing error as Result<U, E>
```

In either case the `Result`is converted from a `Result<T, E>` to a `Result<U, E>`. It's important to note that we stay within a `Result` after running the function `F`. Here's a simple example demonstrating this:

```{.rust .scrollx}
  let result_ok_1: Result<u32, String> = Ok(1);
  let result_ok_2: Result<u32, String> = result_ok_1.map(|n| n * 2); // Ok(2), multiplied by 2
  let result_ok_3: Result<String, String> = result_ok_2.map(|n| format!("age: {}", n)); // Ok("age: 2"), converted to a String

  let result_err_1: Result<u32, String> = Err("You have errors".to_owned());
  let result_err_2: Result<u32, String> = result_err_1.map(|n| n * 2); // Err("You have errors"), no change
  let result_err_3: Result<String, String> = result_err_2.map(|n| format!("age: {}", n)); // Err("You have errors"), no change
```

You can also think of the `map` function as of type: `Result<T -> U, E>`; as in it runs a function on the success side of `Result` leaving the error side untouched.


Continue on to [Combining Results](2024-01-24-working-with-rust-result-part-6.html)
