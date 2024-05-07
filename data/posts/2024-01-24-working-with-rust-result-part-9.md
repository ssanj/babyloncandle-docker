---
title: Working With Rust Result - Combining Results Some More - Part 9
author: sanjiv sahayam
description: Working with Rust Result - Combining Results some more
tags: rust
comments: true
---

There are even more ways to combine `Results`s!

## and

`and` is similar to `and_then` except a default `Result` is returned on an `Ok` instance:

```{.rust .scrollx}
pub fn and<U>(self, res: Result<U, E>) -> Result<U, E> {
    match self {
        Ok(_) => res,
        Err(e) => Err(e),
    }
}
```

Notice that the value inside the `Ok` instance is never used:

```{.rust .scrollx}
Ok(_) => res,
```

Since `res` is [eager](2024-01-24-working-with-rust-result-part-13.html#eager-vs-laziness) it will get evaluated as soon as `and` is called. Values for `res` should only be constants and precomputed values.

In summary:

```{.rust .scrollx}
// pseudocode
// Given: a Result<T, E>
// Return typr: Result<U, E>
// res is eager

Ok(_:T)  -> res:Result<U, E> -> Result<U, E>  // `Ok` value type changes from `T` from `U`
Err(e:E) -> Err(e)           -> Result<U, E>  // Notice that the `Err` value type is fixed at: `E`
```

<img src="/images/2024-01-24-working-with-rust-result/and-2.png" width="600" />

This can be useful when you only want to know if something succeeded or failed instead of needing to work on its value.

Take creating a directory and returning a `Success` value as an example.

```{.rust .scrollx}
enum FileCreation {
  Success,
  Failure
}
```

We can create a directory with the `create_dir` function from the `std::fs` module:

```{.rust .scrollx}
fn create_dir<P: AsRef<Path>>(path: P) -> io::Result<()>
```

Notice how this function returns a `Result` with a `unit` as the success value.

If we use `map` to complete the example use case:

```{.rust .scrollx}
fn create_directory_map(dir_path: &Path) -> io::Result<FileCreation> {
  create_dir(dir_path)
    .map(|_| { // We ignore the value from create_dir
        FileCreation::Success
    }) // Result<FileCreation>
}
```

We have to ignore the previous success value in `map` (as we can't do anything useful with `unit`). This is a little verbose and we can trim it down with `and`:

```{.rust .scrollx}
fn create_directory_and(dir_path: &Path) -> io::Result<FileCreation> {
  create_dir(dir_path)
    .and(Ok(FileCreation::Success))  // Result<FileCreation>
}
```

## or

If you wanted to try an alternative `Result` on `Err` and you didn't care about the error value, you could use `or`. `or` is defined as:

```{.rust .scrollx}
 pub fn or<F>(self, res: Result<T, F>) -> Result<T, F> {
   match self {
     Ok(v) => Ok(v),
     Err(_) => res,
   }
 }
```

In the definition above the value `res` is used only when there is an `Err` instance. If the `Result` is an `Ok` instance, its value
is returned.

Since `res` is [eager](2024-01-24-working-with-rust-result-part-13.html#eager-vs-laziness) it will get evaluated as soon as `or` is called. Values for `res` should only be constants and precomputed values.

In summary:

```{.rust .scrollx}
// pseudocode
// Given: Result<T, E>
// Return type: Result<T, F>
// res is eager

Err(_:E) -> res:Result<T, F>  -> Result<T, F> // The `Err` value type changes from `E` to `F`
Ok(t:T)  -> Ok(t)             -> Result<T, F> // `Ok` value type is fixed: `T`
```

It's important to note that `res` dictates the final `Err` type returned from `or` and that the type inside the `Ok` constructor doesn't change. We'll see that come into play in the example below.

<img src="/images/2024-01-24-working-with-rust-result/or-2.png" width="600" />

Take the example of reading some configuration from a file or returning a default.

We can read from a file with the `read_string` function in the `std::fs` module:

```{.rust .scrollx}
pub fn read_to_string<P: AsRef<Path>>(path: P) -> Result<String>
```

We can read the config file or return a default with:

```{.rust .scrollx}
fn read_config(config_file: &str) -> Result<String, MyError> {
  use std::fs;
  let default_config: String ="verbose=true".to_owned();

  fs::read_to_string(config_file) // Result<String, std::io::Error>
    .or(Ok(default_config)) // Result<String, MyError>
}
```

The function `res`, passed to `or` dictates the final `Err` type. In the above example our error type has changed from `std::io::Error` to `MyError`.
Also when chaining multiple `or` calls, the final `res` block dictates the final `Result` type. In the case of `or` chaining, the `Ok` type is fixed but the `Err` type can vary!

## or_else

`or_else` is similar to `or` with the exception that you get access to the error type `E` and the `op` parameter is lazy:


```{.rust .scrollx}
  pub fn or_else<F, O: FnOnce(E) -> Result<T, F>>(self, op: O) -> Result<T, F> {
      match self {
          Ok(t) => Ok(t),
          Err(e) => op(e),
      }
  }
```

The function `op` takes in the `Err` type `E` and returns a `Result` with the same success type `T` and a new error type `F`:

```{.rust .scrollx}
FnOnce(E) -> Result<T, F>
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given: Result<T, E>
// Return type: Result<T, F>

Err(e:E) -> op(e)  -> Result<T, F> // `Err` value type goes from `E` -> `F`
Ok(t:T)  -> Ok(t)  -> Result<T, F> // `Ok` value type is fixed: `T`
```
<img src="/images/2024-01-24-working-with-rust-result/or-else.png" width="600" />

Here's an example of where we can try one of several parse functions until we find one that succeeds.

Given a common error type `MyError` and a common success type `MyResult`:


```{.rust .scrollx}
#[derive(Debug)]
struct MyError(String);

#[derive(Debug)]
enum MyResult {
  N(u32),
  B(bool),
  S(String),
}
```

And functions to parse numbers and booleans:

```{.rust .scrollx}
fn parse_number(value: &str) -> Result<u32, ParseIntError> {
  u32::from_str(value)
}

fn parse_bool(value: &str) -> Result<bool, ParseBoolError> {
  bool::from_str(value)
}
```

One thing to note is that both functions return different error types in `Err`: `ParseIntError` and [ParseBoolError](https://doc.rust-lang.org/std/str/struct.ParseBoolError.html) respectively.

How would we combine these functions into parsing a string slice into a type of `MyResult`? And we also don't support converting a string that is all caps into `MyResult`. That would be an error.

Similar to `or`, the function `op`, passed to `or_else` dictates the final `Err` type. When chaining multiple `or_else` calls, the final `op` call dictates the final `Result` type. In the case of `or_else` chaining, the `Ok` type is fixed but the `Err` type can vary.

`Note` that we don't need to align the error types here as mentioned before because the `Result` passed to `or_else` would change the final `Err` type as required.

Here's one way we could do it:

```{.rust .scrollx}
fn parse_my_result(value: &str) -> Result<MyResult, MyError> {
  println!("value: {}", value);
  parse_number(value)
    .map(|n| MyResult::N(n))
    .or_else(|_|
      parse_bool(value)
        .map(|b| MyResult::B(b))
    )
    .or_else(|_|
      if value.to_uppercase() == value {
          println!("second or if");
          // We don't support full screaming caps
          Err(MyError(format!("We don't support screaming case: {}", value)))
       } else {
        println!("second or else");
        Ok(MyResult::S(value.to_owned()))
       }
    )
}
```

We could use it like:

```{.rust .scrollx}
let r1: Result<MyResult, MyError> = parse_my_result("123"); // Ok(N(123))
let r2: Result<MyResult, MyError> = parse_my_result("true"); // Ok(B(true))
let r3: Result<MyResult, MyError> = parse_my_result("something"); //Ok(S("something"))
let r4: Result<MyResult, MyError> = parse_my_result("HELLO"); //Err(MyError("We don't support screaming case: HELLO"))
```

How the `Err` type changed between `ParseIntError`, `ParseBoolError` to `MyError` can be a bit harder to see. Here's a more detailed example of the above:

```{.rust .scrollx}
fn parse_my_result_2(value: &str) -> Result<MyResult, MyError> {
  let p1: Result<MyResult, ParseIntError> = parse_number(value)
    .map(|n| MyResult::N(n));

  let p2: Result<MyResult, ParseBoolError> =  parse_bool(value)
        .map(|b| MyResult::B(b));

  let p3: Result<MyResult, MyError> =
    if value.to_uppercase() == value {
        // We don't support full screaming caps
        Err(MyError(format!("We don't support screaming case: {}", value)))
     } else {
      Ok(MyResult::S(value.to_owned()))
     };

    let r1: Result<MyResult, ParseBoolError> = p1.or_else(|_| p2);
    let r2: Result<MyResult, MyError> = r1.or_else(|_|p3);

    r2
}
```

Would could also write the above example with `or` since we have precomputed all the values before using `or_else`:

```{.rust .scrollx}
fn parse_my_result_3(value: &str) -> Result<MyResult, MyError> {
  let p1: Result<MyResult, ParseIntError> = parse_number(value)
    .map(|n| MyResult::N(n));  // Already evaluated

  let p2: Result<MyResult, ParseBoolError> =  parse_bool(value)
        .map(|b| MyResult::B(b));  // Already evaluated

  let p3: Result<MyResult, MyError> =
    if value.to_uppercase() == value {
        // We don't support full screaming caps
        Err(MyError(format!("We don't support screaming case: {}", value)))
     } else {
      Ok(MyResult::S(value.to_owned()))
     }; // Already evaluated

    let r1: Result<MyResult, ParseBoolError> = p1.or(p2); // Using or
    let r2: Result<MyResult, MyError> = r1.or(p3);  // Using or

    r2
}
```

- Continue on to [Working with Errors](2024-01-24-working-with-rust-result-part-10.html)
- Back to [TOC](2024-01-24-working-with-rust-result.html)
