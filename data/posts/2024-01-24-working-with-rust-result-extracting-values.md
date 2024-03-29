---
title: Working With Rust Result - Extracting Values from a Result
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---


## Pattern Matching

Since `Rust` supports [pattern matching](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html), a simple way to extract a value from a `Result` is to pattern match:

```{.rust .scrollx}
fn print_age(age_result: Result<u8, String>) {
  match age_result {
    Ok(age)    => println!("You are twenty five!"), // We could also do something with 'age' if we wanted.
    Err(error) => println!("Imposter! {}", error),
  }
}

print_age(twenty_five(20)); // Imposter! 20 is not 25!
print_age(twenty_five(25)); // You are twenty five!
```

## map_or_else

Another way to extract the values from a `Result` is to use the `map_or_else` function:

```{.rust .scrollx}
pub fn map_or_else<U, D: FnOnce(E) -> U, F: FnOnce(T) -> U>(self, default: D, f: F) -> U {
    match self {
        Ok(t) => f(t),
        Err(e) => default(e),
    }
}
```

In the above definition we get two functions: `D` and `F`. Both functions convert a value into the same result type `U`:

```{.rust .scrollx}
// pseudocode

F: T -> U // Convert success value to a U
D: E -> U // Convert error value to a U
```

 `D` is used on the error value inside an `Err` instance and `F` is used on the success value inside an `Ok` instance. `map_or_else` has simply run a function on each data constructor (`Ok` and `Err`) to produce a result of the same type in all cases: `U`.

## map_or

Another function in the same family of functions is `map_or`:

```{.rust .scrollx}
pub fn map_or<U, F: FnOnce(T) -> U>(self, default: U, f: F) -> U {
    match self {
        Ok(t) => f(t),
        Err(_) => default, // We ignore the value and return a default
    }
}
```

In the above definition, a function `F` runs on the value inside the `Ok` instance and a default value is returned if it's an `Err` instance:

```{.rust .scrollx}
// pseudocode

F      : T -> U // Convert success value to a U
default:   -> U // Return a U if in error
```

Notice that we completely ignore the value inside of the `Err` instance.

`map_or` differs from `map_or_else`, in that it only takes a single function `F` and a default value to return in the `Err` case. This can be useful if you don't care about what the error case was and simple want to return some default value.


Continue on to [Being Unsafe](2024-01-24-working-with-rust-result-extracting-values-unsafe.html)
