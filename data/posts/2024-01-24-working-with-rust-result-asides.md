---
title: Working With Rust Result - Asides
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---

## Functions that return Result in std

Many of the `std` functions return `Result`s if the action you're trying to perform can fail. Here's
an example from the [Write](https://doc.rust-lang.org/std/io/trait.Write.html) trait in `std::io`:

```{.rust .scrollx}
trait Write {
  fn write_all(&mut self, buf: &[u8]) -> Result<()>
  ...
}
```

Using the `write_all` method to write the contents of the supplied buffer (`buf`) can fail with IO errors, or it could succeed in which case a Unit (`()`) value is returned.

Now hold on, `Result` should have two type variables and the example above clearly only has one. What's going on?

A frequent pattern used in Rust libraries is to create a type alias for `Result` that wraps a particular error type. In the example above, `Result` is [aliased](https://doc.rust-lang.org/std/io/type.Result.html) as follows:

```{.rust .scrollx}
pub type Result<T> = Result<T, Error>;
```

Where `Error` is a [std::io::Error](https://doc.rust-lang.org/std/io/struct.Error.html):

```{.rust .scrollx}
pub struct Error { /* private fields */ }
```

essentially giving you:

```{.rust .scrollx}
pub type Result<T> = Result<T, std::io::Error>;
```

With a type alias like above, we don't have to constantly specify a type for a `Result`'s error. This is useful where a particular module usually returns the same error type for most or all of its functions. For example, all `std::io` functions that return `Result` use `std::io::Error` as the error type.


## Strict vs Laziness


Another difference is that, the function supplied will not get called unless there is an `Err` value to call it with. This is different to `unwrap_or`'s default value which is evaluated on `Ok` values as well:

```{.rust .scrollx}
  let strict_result_ok: Result<u32, String> = Ok(1);
  let strict_result_err: Result<u32, String> = Err("You have errors".to_owned());

  strict_result_ok.unwrap_or(panic!("boom")); // panics even though this is an `Ok`
  strict_result_ok.unwrap_or_else(|_| panic!("boom")); // does not panic because this is an `Ok`
  strict_result_err.unwrap_or_else(|_| panic!("boom")); // panics on `Err` as expected
```

<p/>

You can think of `unwrap_or` as being "strict" or "eager" in its evaluation of the `default` parameter - it always evaluates the default value on `Ok` and `Err`. `unwrap_or_else` can be thought of as "lazy" or "evaluated when needed" - it only runs when the value returned is an `Err`.

<p/>

In general prefer `unwrap_or_else` if you don't want your code running until there is an `Err`. `unwrap_or` is fine if your default value is a constant or has been already evaluated. This distinction applies to all `_or` and `_or_else` variants.

Continue on to [summary](2024-01-24-working-with-rust-result-summary.html)
