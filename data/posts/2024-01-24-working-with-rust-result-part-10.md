---
title: Working With Rust Result - Working with Errors - Part 10
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---

The following functions help us work on the error side of a `Result`.

### map_err

You often need to map the error type on `Result` to another. To do so we can use the `map_err` function:

```{.rust .scrollx}
pub fn map_err<F, O: FnOnce(E) -> F>(self, op: O) -> Result<T, F> {
  match self {
      Ok(t) => Ok(t),
      Err(e) => Err(op(e)),
  }
}
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Err(e:E) -> op(e)  -> F  -> Result<T, F>  // `Err` type goes from `E` -> `F` and is wrapped in an `Err`
Ok(t:T)  -> Ok(t)        -> Result<T, F> // `Ok` type is fixed: `T`
```

Here's an example where we need to convert a `ParseBoolError` type to `MyError`:

```{.rust .scrollx}
fn bool_as_my_error(value: &str) -> Result<bool, MyError> {
  parse_bool(value)
    .map_err(|e| MyError(e.to_string())) //Convert ParseBoolError -> MyError
}
```

`map_err` is useful when you need to align all the error types to a single type like when using the question mark operator or
when using functions like `and_then`.

### unwrap_err

`unwrap_err` gives you access to the error inside an `Err` instance and <u>panic</u>s on an `Ok` instance. This is an unsafe function
and should be used only when you know for certain that you have an `Err` or don't care (like maybe in a test function, where you want the test to fail).

```{.rust .scrollx}
pub fn unwrap_err(self) -> E
where
    T: fmt::Debug,
{
    match self {
        Ok(t) => unwrap_failed("called `Result::unwrap_err()` on an `Ok` value", &t),
        Err(e) => e,
    }
}
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Err(e:E) -> E     // Returns the error in the Err
Ok(_)    -> panic // Panics on any Ok value
```

For example, if we try to unwrap a success value:

```{.rust .scrollx}
parse_bool("true").unwrap_err() // panics - called `Result::unwrap_err()` on an `Ok` value: true
```

when used on an error:

```{.rust .scrollx}
parse_bool("ten").unwrap_err() // ParseBoolError
```

### expect_err

`expect_err` is similar to `unwrap_err` where you get the value inside the `Err` instance or <u>panic</u>, but where you get to provide a custom error message.

`expect_err` is defined as:

```{.rust .scrollx}
pub fn expect_err(self, msg: &str) -> E
    where
        T: fmt::Debug,
    {
        match self {
            Ok(t) => unwrap_failed(msg, &t),
            Err(e) => e,
        }
    }
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Err(e:E) -> E     // Returns the error in the Err
Ok(_)    -> panic // Panics on any Ok value with the supplied message
```

For example, if we try to unwrap a success value:

```{.rust .scrollx}
parse_bool("true").expect_err("This should not be bool") // panics - This should not be bool: true
```

when used on an error:

```{.rust .scrollx}
parse_bool("ten").expect_err("This should not be bool") // ParseBoolError
```

## Conversions to Option

### ok

Sometimes it's useful to convert a `Result` to an `Option`. `ok` maps an `Ok` instance to a `Some` instance and an `Err`
instance to a `None`:

```{.rust .scrollx}
pub fn ok(self) -> Option<T> {
  match self {
      Ok(x) => Some(x),
      Err(_) => None,
  }
}
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Ok(t:T) -> Some(t) // Option<T>
Err(_)  -> None    // Option<T>
```

For example, to only get a list of valid numbers from a list of valid and invalid numbers, we could use:

```{.rust .scrollx}
  let maybe_numbers =
    vec![
      "1",
      "2",
      "three",
      "4"
    ];

    maybe_numbers
      .iter()
      .filter_map(|maybe_number| {
          parse_number(maybe_number).ok()
      })
      .collect::<Vec<_>>(); // [1, 2, 4]
```

### err

This is the opposite of `ok` where we reverse the mappings, going from an `Ok` instance to `None` and an `Err` instance to
`Some`:

```{.rust .scrollx}
pub fn err(self) -> Option<E> {
  match self {
      Ok(_) => None,
      Err(x) => Some(x),
  }
}
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Err(e:E) -> Some(e) // Option<E>
Ok(_)    -> None    // Option<E>
```
For example, if we only wanted invalid numbers from our list of possible numbers, we could use:

```{.rust .scrollx}
let only_errors =
  maybe_numbers
    .iter()
    .filter_map(|maybe_number| {
        parse_number(maybe_number)
          .err()
          .map(|e| (maybe_number, e)) // Return a pair of the "number" and the error
    })
    .collect::<Vec<_>>(); // [("three", ParseIntError { kind: InvalidDigit })]
```

### transpose

If we have a `Result` with an `Option` value inside an `Ok` instance, we can use `transpose` to convert it to an `Option`
with a `Result` within the `Some` instance! `Result` has an `impl` of `Result<Option<T>, E>` that defines the `transpose` function:

```{.rust .scrollx}
impl<T, E> Result<Option<T>, E> {

  pub const fn transpose(self) -> Option<Result<T, E>> {
    match self {
      Ok(Some(x)) => Some(Ok(x)),
      Ok(None) => None,
      Err(e) => Some(Err(e)),
    }
  }
}
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<Option<T>, E>

Ok(Some(t:T))  -> Some(Ok(t))    // Option<Result<T, E>>
Ok(None)       -> None           // Option<Result<T, E>>
Err(e:E)       -> Some(Err(e))   // Option<Result<T, E>>
```

We are basically flipping the containers; going from `Result<Option<T>, E>` to a `Option<Result<T, E>>`.

But why is this useful?

This can be useful when you have a one or more `Result<Option<T>, E>` and want to know if all the inner `Option` types are
valid `Some` instances. For example, to retrieve only even numbers or any parse errors we could use:

```{.rust .scrollx}
let maybe_numbers_2 =
  vec![
    "1",
    "2",
    "three",
    "4",
    "5",
    "6",
    "se7en",
  ];

maybe_numbers_2
  .iter()
  .filter_map(|maybe_number| {
      parse_number(maybe_number)
        .map(|n| {
          if n % 2 == 0 {
            Some(n) // We only want even numbers
          } else {
            None // We want odd numbers filtered out
          }
        })
        .transpose()
  })
  .collect::<Vec<_>>() // [Ok(2), Err(ParseIntError { kind: InvalidDigit }), Ok(4), Ok(6), Err(ParseIntError { kind: InvalidDigit })]
```


## Value tests

### is_ok

When you need to just know if some execution was successful and don't need to know the value, you can use `is_ok`:

```{.rust .scrollx}
pub const fn is_ok(&self) -> bool {
  matches!(*self, Ok(_))
}
```

[matches!](https://doc.rust-lang.org/std/macro.matches.html) is a macro that tests if a value matches a given pattern, returning a bool value to indicate success or failure.

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Ok(_)  -> true  // bool
Err(_) -> false // bool
```

For example:

```{.rust .scrollx}
parse_bool("ten").is_ok();  // false
parse_bool("true").is_ok(); // true
```

We could use this function when testing for conditions:

```{.rust .scrollx}
if parse_bool(value).is_ok() {
  // Do something when we have booleans
} else {
  // Do something when we don't have booleans
}
```

### is_err

This is similar to `is_ok` but in reverse:

```{.rust .scrollx}
pub const fn is_err(&self) -> bool {
  !self.is_ok()
}
```

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Ok(_)  -> false  // bool
Err(_) -> true   // bool
```

For example:

```{.rust .scrollx}
parse_bool("ten").is_err();  // true
parse_bool("true").is_err(); // false
```

In a conditional as before:

```{.rust .scrollx}
if parse_bool(value).is_err() {
  // Do something when we don't have booleans
} else {
  // Do something when we have booleans
}
```


### is_ok_and

`is_ok_and` lets you test a `Result` is `Ok` and runs a predicate on the value inside the `Ok` instance. `is_ok_and` is defined as:

```{.rust .scrollx}
pub fn is_ok_and(self, f: impl FnOnce(T) -> bool) -> bool {
  match self {
    Err(_) => false,
    Ok(x) => f(x),
  }
}
```

We can see from the above definition that the predicate function `f` is only called on the value inside the `Ok` instance, converting it to a boolean value. If the `Result` is an instance of `Err` the value `false` is returned.

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

f: T    -> bool
Ok(t:T) -> f(t)   -> true|false  // bool
Err(_)  -> false                 // bool
```

A simple example is testing whether a number is greater than 10:

```{.rust .scrollx}
parse_int("11").is_ok_and(|n| n > 10)   // true
parse_int("2").is_ok_and(|n| n > 10)    // false
parse_int("blah").is_ok_and(|n| n > 10) // false
```


### is_err_and

`is_err_and` is the opposite of `is_ok_and` in that it lets you test a `Result` is an `Err` and runs a predicate on the value inside the `Err` instance. `is_err_and` is defined as:


```{.rust .scrollx}
pub fn is_err_and(self, f: impl FnOnce(E) -> bool) -> bool {
  match self {
    Ok(_) => false,
    Err(e) => f(e),
  }
}
```

We can see from the above definition that the predicate function `f` is only called on the value inside the `Err` instance, converting it to a boolean value. If the `Result` is an instance of `Ok` the value `false` is returned.

In summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

f: E     -> bool
Ok(_)    -> false                // bool
Err(e:E) -> f(e) -> true|false   // bool
```

A simple example is to testing whether a number is an invalid digit:

```{.rust .scrollx}
  parse_number("2")
    .map_err(|e| MyError(e.to_string()))
    .is_err_and(|MyError(error)| error.contains("invalid digit")); // false

  parse_number("blah")
    .map_err(|e| MyError(e.to_string()))
    .is_err_and(|MyError(error)| error.contains("invalid digit")); // true
```

## Summary

If you've made it this far, you're probably overwhelmed by all the different methods and their uses.
It helps to try and learn and use them one at a time as and when needed. The following table summarises which method you would use under different circumstances.

<table>
  <tbody>
    <tr>
      <th>What do you want to do?</th>
      <th align="right">Method to use</th>
    </tr>
    <tr>
      <td align="center">Create a `Result`</td>
      <td align="left">
        <ul>
          <li>Constructor: `Ok(value)`</li>
          <li>Constructor: `Err(value)`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Get the value out of a `Result`</td>
      <td align="left">
        <ul>
          <li>`Patten matching`</li>
          <li>`map_or_else`</li>
          <li>`map_or`</li>
          <li>`unwrap` unsafe</li>
          <li>`unwrap_or`</li>
          <li>`unwrap_or_else`</li>
          <li>`unwrap_or_default`</li>
          <li>`expect` unsafe</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Run a function on the value inside `Ok`</td>
      <td align="left">
        <ul>
          <li>`map`</li>
          <li>`and_then`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Run a function on the value inside `Err`</td>
      <td align="left">
        <ul>
          <li>`or_else`</li>
          <li>`map_err`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Get the value inside `Ok`</td>
      <td align="left">
        <ul>
          <li>`unwrap` unsafe</li>
          <li>`expect` unsafe</li>
          <li>`? operator`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Get the value inside `Ok` with fallback</td>
      <td align="left">
        <ul>
          <li>`Patten matching`</li>
          <li>`map_or`</li>
          <li>`unwrap_or`</li>
          <li>`unwrap_or_else`</li>
          <li>`unwrap_or_default`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Get the value inside `Err`</td>
      <td align="left">
        <ul>
          <li>`unwrap_err` unsafe</li>
          <li>`expect_err` unsafe</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Get the value inside `Err` with fallback</td>
      <td align="left">
        <ul>
          <li>`Patten matching`</li>
          <li>`map_or`</li>
          <li>`unwrap_or`</li>
          <li>`unwrap_or_else`</li>
          <li>`unwrap_or_default`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Combine two `Result`s that are `Ok`</td>
      <td align="left">
        <ul>
          <li>`and_then`</li>
          <li>`and`</li>
          <li>`? operator`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Combine two `Result`s that are `Err`</td>
      <td align="left">
        <ul>
          <li>`or`</li>
          <li>`or_else`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Convert `Ok` to `Option` as `Some`</td>
      <td align="left">
        <ul>
          <li>`ok`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Convert `Err` to `Option` as `Some`</td>
      <td align="left">
        <ul>
          <li>`err`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Convert `Result<Option>` to `Option<Result>`</td>
      <td align="left">
        <ul>
          <li>`transpose`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Test for `Ok`</td>
      <td align="left">
        <ul>
          <li>`is_ok`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Test for `Err`</td>
      <td align="left">
        <ul>
          <li>`is_err`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Test for `Ok` and run a predicate</td>
      <td align="left">
        <ul>
          <li>`is_ok_and`</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td align="center">Test for `Err` and run a predicate</td>
      <td align="left">
        <ul>
          <li>`is_err_and`</li>
        </ul>
      </td>
    </tr>
  </tbody>
</table>
