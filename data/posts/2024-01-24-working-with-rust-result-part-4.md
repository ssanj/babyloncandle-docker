---
title: Working With Rust Result - Extracting Values from a Result Safely - Part 4
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---


We can `unwrap` a `Result` safely, if we provide a default value of type `T` or call a function that returns a value of type `T` when given a type `E`:

```{.rust .scrollx}
// pseudocode
// Given: Result<T, E>

Ok(T)  -> T
Err(E) -> T // return value of `T` or a use a function of type `E` -> `T`
```

### unwrap_or

`unwrap_or` is defined as:
`unwrap_or` is defined as:

```{.rust .scrollx}
pub fn unwrap_or(self, default: T) -> T {
    match self {
        Ok(t) => t,
        Err(_) => default,
    }
}
```

In the above definition we supply a default value of type `T`. This default value will be used when there is an `Err`, the `Ok` value will be returned otherwise. This is very similar to `map_or` but where we don't run a function on the success value:

```{.rust .scrollx}
// pseudocode

Ok(t)   ->  t       // Return value in Ok
Err(e)  ->  default // Return default if in error
```

Here's an example of using `unwrap_or` to do just that:

```{.rust .scrollx}
let twenty_five_or_ten_1: u8 = twenty_five(20).unwrap_or(10); // 10
let twenty_five_or_ten_2: u8 = twenty_five(25).unwrap_or(10); // 25
```

### unwrap_or_else

There's a similarly named function called `unwrap_or_else`. The main difference being that `unwrap_or_else` takes in a function that is called when an `Err` is returned:

```{.rust .scrollx}
pub fn unwrap_or_else<F: FnOnce(E) -> T>(self, op: F) -> T {
    match self {
        Ok(t) => t,
        Err(e) => op(e),
    }
}
```

```{.rust .scrollx}
// pseudocode

Ok(t)   ->  t    -> T // Return value in Ok
Err(e)  ->  F(e) -> T // Call F on the error
```

This is very similar to the `map_or_else` function but where a function is only applied to the error case and not the success case.

### unwrap_or_default

Another in the same family of functions is `unwrap_or_default`, which is defined as:

```{.rust .scrollx}
pub fn unwrap_or_default(self) -> T
where
    T: Default,
{
    match self {
        Ok(x) => x,
        Err(_) => Default::default(),
    }
}
```

In the above definition, if a `Result` is an `Err` then the default instance of type `T` is used. The type `T` has a constraint on it that requires that it has an instance of the [Default](https://doc.rust-lang.org/std/default/trait.Default.html) trait: `T: Default`. Here's an example of how to use it:

```{.rust .scrollx}
  let result_ok: Result<u32, String> = Ok(1);
  let result_err: Result<u32, String> = Err("You have errors".to_owned());

  result_ok.unwrap_or_default();  // 1
  result_err.unwrap_or_default(); // 0
```

This is also very similar to `unwrap_or` where, we supply a default value for the error case. In `unwrap_or_default` the default value is derived from the `Default` instance for type `T`:

```{.rust .scrollx}
// pseudocode

Ok(t)   ->  t  -> T // Return value in Ok
Err(_)         -> T // Return Default instance for T
```

Continue on to [Transforming Values](2024-01-24-working-with-rust-result-part-5.html)
