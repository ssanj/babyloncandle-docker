---
title: Working With Rust Result - Combining Results the Question Mark Operator - Part 8
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---

Lets try to perform a calculation on multiple numbers parsed from strings:

```{.rust .scrollx}
let numbers_1: Result<u32, ParseIntError> = add_numbers("10", "20", "30"); // Ok(60)
let numbers_2 = add_numbers("ten", "20", "30");    // Err(ParseIntError { kind: InvalidDigit })
let numbers_3 = add_numbers("10", "twenty", "30"); // Err(ParseIntError { kind: InvalidDigit })
let numbers_4 = add_numbers("10", "20", "thirty"); // Err(ParseIntError { kind: InvalidDigit })
```

Here's the definition of `add_numbers`:

```{.rust .scrollx}
fn add_numbers(one: &str, two: &str, three: &str) -> Result<u32, ParseIntError> {
  parse_number(one) // try and get the first number. Returns Result<u32, ParseIntError>
    .and_then(|n1| { // if that succeeds,
      parse_number(two) // try and get the second number. Returns Result<u32, ParseIntError>
        .and_then(|n2| { // if that succeeds
          parse_number(three) // try and get the third number. Returns Result<u32, ParseIntError>
            .map(|n3| n1 + n2 + n3) // if that succeeds, add up all the previous numbers. Returns Result<u32, ParseIntError>
        })
    })
}
```

This is similar to how we previously parsed two numbers. This is quickly becoming hard to reason about. Parsing more numbers like this would be almost unmaintainable. Luckily Rust gives us a simpler way to do this.


## The question mark operator

Rust has the [question mark operator](https://doc.rust-lang.org/reference/expressions/operator-expr.html#the-question-mark-operator) (`?`) which allows you to simply
return an error or extract a success value. You can think of it as an `unwrap` on `Ok` with an immediate return on `Err`, instead of panic-ing.

<img src="/images/2024-01-24-working-with-rust-result/question-mark-operator.png" width="600" />

Here's another the definition of `and_numbers` which uses the `?` operator:

```{.rust .scrollx}
fn add_numbers_2(one: &str, two: &str, three: &str) -> Result<u32, ParseIntError> {
  let n1: u32 = parse_number(one)?;   // Get the number or return an Err
  let n2: u32 = parse_number(two)?;   // Get the number or return an Err
  let n3: u32 = parse_number(three)?; // Get the number or return an Err

  // If we got here, all the numbers are valid
  Ok(n1 + n2 + n3) // Add all the numbers and return an Ok
}
```

It's important to note that if any of the `parse_number` function calls return an `Err`, the `add_numbers_2` function would return that `Err` as the final result instead of proceeding to the next line.

> We have to still wrap the final result in an `Ok` constructor as `add_numbers_2` returns a `Result<u32, ParseIntError>`.

We can see that the `add_numbers_2` function is easier to reason about than chaining together `and_then` and `map` calls as in the `add_numbers` function. The `?` operator is supported for `Result` and `Option` types at the moment.



Continue on to [combining some more](2024-01-24-working-with-rust-result-part-9.html)
