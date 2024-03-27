---
title: Working With Rust Result
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---

In Rust there is a type to to represent a value that maybe successful or in error. In Rust this type is called a [Result](https://doc.rust-lang.org/std/result/index.html) and is defined as:

```{.rust .scrollx}
enum Result<T, E> {
   Ok(T),
   Err(E),
}
```


The `Ok` constructor is used to represent success and the `Err` constructor is used to present an error.
The first type variable `T` represents the success **value** while the second type `E` represents the error **value**:

```{.rust .scrollx}
/// T -> Success type
/// E -> Error type
enum Result<T, E> {
   Ok(T),  // All good
   Err(E), // Oh noes
}
```

<img src="/images/2024-01-24-working-with-rust-result/rust-result-structure.png" width="600" />

> Note: The code examples from the Rust std are from version `1.77.0`.

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

## Construction

Let's look at how to construct success and error values using `Result`. We mentioned that we use the `Ok` constructor for success values and the `Err` constructor
for errors. Here's an example of using the constructors to validate that an age is twenty five:

```{.rust .scrollx}
fn twenty_five(age: u8) -> Result<u8, String> {
  if age == 25 {
    Ok(age)
  } else {
    Err(format!("{} is not 25!", age))
  }
}
```

From the above `Result` type:

```{.rust .scrollx}
 Result<u8, String>
```

we know that a `u8` will be the success type (`T`) and a `String` will be the error type (`E`).

`Note`: It's not recommended to use `String`s for error values because the compiler doesn't
help you if you forget to handle a particular `String`. A better alternative is to use an `enum` of error values. We'll see an example of that later.

If we wanted to print the output of calls to the above function, we could do something like this:

```{.rust .scrollx}
println!("{:?}", twenty_five(5));  // Err("5 is not 25!")
println!("{:?}", twenty_five(25)); // Ok(25)
println!("{:?}", twenty_five(35))  // Err("35 is not 25!")
```

We have used the debug syntax (`{:?}`) to display the value of the `Result` in the above example. What if we want to actually get the success or error value "out" of a `Result` instead of just printing it?

## Extracting values from a Result

### Pattern Matching

Since `Rust` supports [pattern matching](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html), a simple way to extract a value from a `Result` is to pattern match:

```{.rust .scrollx}
fn print_age(age_result: Result<u8, String>) {
  match age_result {
    Ok(age)    => println!("You are twenty five!"), // we could also do something with 'age' if we wanted.
    Err(error) => println!("Imposter! {}", error),
  }
}

print_age(twenty_five(20)); // Imposter! 20 is not 25!
print_age(twenty_five(25)); // You are twenty five!
```

### map_or_else

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

 `D` is used on the error value inside an `Err` instance and `F` is used on the success value inside an `Ok` instance. `map_or_else` has simply run a function on each data constructor (`Ok` and `Err`) to produce a result of the same type in all cases.

### map_or

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


### Being unsafe

`Note`: Do not use these functions if you have better/safer alternatives.


#### unwrap

What if we want to fail (panic) our program if the supplied age is not twenty five?

We can work unsafely by using `unwrap`. `unwrap` is defined as:

```{.rust .scrollx}
pub fn unwrap(self) -> T
where
    E: fmt::Debug,
```

We can see that the above definition returns the success type `T` under all conditions. But how can it return a success value `T` if it's an `Err` instance with a value of type `E`?

Unwrap's implementation demonstrates how this is achieved:

```{.rust .scrollx}
pub fn unwrap(self) -> T
where
    E: fmt::Debug,
{
    match self {
        Ok(t) => t,
        Err(e) => unwrap_failed("called `Result::unwrap()` on an `Err` value", &e),
    }
}

fn unwrap_failed(msg: &str, error: &dyn fmt::Debug) -> ! {
    panic!("{msg}: {error:?}")
}
```

Since we don't have some sort of default value for `T` supplied, this function `panic`s when the result is an `Err`:

```{.rust .scrollx}
let twenty_five_1: u8 = twenty_five(25).unwrap(); // This works because the result is 'Ok'

let twenty_five_2: u8 = twenty_five(20).unwrap(); // This goes boom! because the result is 'Err'
//thread 'main' panicked at src/main.rs:9:22:
//called `Result::unwrap()` on an `Err` value: "20 is not 25!"
```

Also note that the error `E` has to have an instance of the `Debug` trait. This is so that the error can be written out if the `unwrap` causes a `panic`:

```{.terminal .scrollx}
called `Result::unwrap()` on an `Err` value: "20 is not 25!"
```

#### expect
What if we wanted to customize the error message when we failed?

We can do that by using `expect` method. `expect` is defined as:

```{.rust .scrollx}
pub fn expect(self, msg: &str) -> T
where
    E: fmt::Debug,
{
    match self {
        Ok(t) => t,
        Err(e) => unwrap_failed(msg, &e),
    }
}
```

Similar to the definition for `unwrap`, a success type of `T` is always returned or the function panics:

```{.rust .scrollx}
let twenty_five_1: u8 = twenty_five(25).expect("Ooops! Looks like you're not twenty five"); // This works because the result is 'Ok'

let twenty_five_2: u8 = twenty_five(20).expect("Ooops! Looks like you're not twenty five"); // This goes boom! because the result is 'Err'
//thread 'main' panicked at src/main.rs:9:22:
//Ooops! Looks like you're not twenty five: "20 is not 25!"
```

It's important to note that the value in the `Err`: "20 is not 25!" is still printed but we get to customize the message preceding it:

```{.terminal .scrollx}
Ooops! Looks like you're not twenty five
```

Panic-ing your program is probably the last thing you want to do; It's something you do when you have no other options. As such it's highly discouraged. Wouldn't it be better to calmly handle any errors and exit gracefully?

But how do you do that? We've already seen some ways to do that with pattern matching, `map_or_else` and `map_or`. There are other ways which we will look at next.

### Making things safer with defaults and fallbacks

One way we can `unwrap` a `Result` safely, is to provide a default value or function, that returns a value of type `T` (The `Ok` type) when there is an `Err`.

#### unwrap_or

`unwrap_or` is defined as:

```{.rust .scrollx}
pub fn unwrap_or(self, default: T) -> T {
    match self {
        Ok(t) => t,
        Err(_) => default,
    }
}
```

We can see from the above definition that, we supply a default value of `T`. This default value will be used when there is an `Err`, the `Ok` value will be returned otherwise. This is very similar to [map_or](#map_or) but where we don't run a function on the success value:

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

#### unwrap_or_else

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

This is very similar to the [map_or_else](#map_or_else) function but where a function is only applied to the error case and not the success case.

<details>
  <summary>Strict vs Laziness</summary>

<p/>
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
</details>

#### unwrap_or_default

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

This is also very similar to the `unwrap_or` where we supply a default value for the error case. In `unwrap_or_default` the default value is derived from the `Default` instance for type `T`:

```{.rust .scrollx}
// pseudocode
Ok(t)   ->  t  -> T // Return value in Ok
Err(_)         -> T // Return Default instance for T
```

## Transforming values within a Result

In the functions above, we extracted the success and error values out of a `Result`, thereby losing our `Result` "container". What if you could run a function on the value within a `Result` and stay within the `Result` "container"? Then you wouldn't have to do all the pesky unwrapping until when you actually needed the value.

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

We can see from the above definition that, the supplied function `F` is only run on the `Ok` instance and the `Err` instance is left untouched.

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


## Combining Results for fun and profit

`Result` gets interesting when you need to combine multiple of them to give you one final `Result`.

### and_then

Sometimes when you have multiple functions that return `Result`s, you want to know if all of them succeeded or any of them failed. `and_then` can help you there. `and_then` is defined as:

```{.rust .scrollx}
pub fn and_then<U, F: FnOnce(T) -> Result<U, E>>(self, op: F) -> Result<U, E> {
    match self {
        Ok(t) => op(t),
        Err(e) => Err(e),
    }
}
```

From the above definition, the function `F` is run on the success value within an `Ok` instance. This is very similar to [map](#map). The main difference is that the function `F` returns another `Result` instead of another type.

```{.rust .scrollx}
// pseudocode
// Given: Result<T, E>
F: T -> Result<U, E> // Converts a success value into another Result

Ok(t:T)   ->  F(t)  -> Ok(U)  // Return converted value in Ok, as a Result<U, E>
Err(e:E)            -> Err(e) // Return existing error as Result<U, E>
```

Given the following function that parses a string to a `u32` or a [ParseIntError](https://doc.rust-lang.org/std/num/struct.ParseIntError.html):

```{.rust .scrollx}
use std::num::ParseIntError;
use std::str::FromStr;

fn parse_number(value: &str) -> Result<u32, ParseIntError> {
  u32::from_str(value)
}
```

Let's try and parse a string number with `parse_number` and multiply its value by 2:

```{.rust .scrollx}
parse_number("10")
    .and_then(|ten| {
        // We have successfully parsed "10" into 10.
        let new_result = ten * 2; // Multiply by 2
        todo!() // What do we return here?
    })
```

Given that we have to use a function that also returns a `Result` from `and_then`:

```{.rust .scrollx}
parse_number("10")
    .and_then(|ten| {
        // We have successfully parsed "10" into 10.
        let new_result = ten * 2 // Multiply by 2
        Ok(new_result) // Result<u32, ParseIntError>
    })
```

If we want to fail our calculation for some reason we can return an `Err`:

```{.rust .scrollx}
struct MyError(String);

parse_number("10")
.and_then(|one| {
    // We have successfully parsed "10" into 10.
    parse_number("20")
      .and_then(|two| {
          // We have successfully parsed "20" into 20.
          // but we don't like even numbers...
          if two % 2 == 0 {
              Err(MyError("I don't add even numbers".to_owned()))
          } else {
              Ok(one + two)
          }
      })
})
```

But we can an error!:

```{.terminal .scrollx}
error[E0308]: mismatched types
   --> src/main.rs:86:23
    |
86  |                   Err(MyError("I don't add even numbers".to_owned()))
    |                   --- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expected `ParseIntError`, found `MyError`
    |                   |
    |                   arguments to this enum variant are incorrect
    |
help: the type constructed contains `MyError` due to the type of the argument passed
   --> src/main.rs:86:19
    |
86  |                   Err(MyError("I don't add even numbers".to_owned()))
    |                   ^^^^----------------------------------------------^
    |                       |
    |                       this argument influences the type of `Err`
```

which points to the real cause:

> expected `ParseIntError`, found `MyError`


What this means is that when you are chaining `Result`s through `and_then` functions, all the `Err` types need to be the same. We can change the `Ok` type as
much as we want but we have to `align` the errors. This is just something to keep in mind when using `Result`. If you have functions that return `Result`s with different
`Err` types, you can create a common error type and convert each error into that type using something like [map_err](#map_err), which we will cover later.


For completeness, here's how you align your error types with `map_error`:

```{.rust .scrollx}
parse_number("10")
  .map_err(|e| MyError(e.to_string())) // Result<u32, MyError>
  .and_then(|one| {
    // We have successfully parsed "10" into 10.
    parse_number("20")
      .map_err(|e| MyError(e.to_string())) // Result<u32, MyError>
      .and_then(|two| {
          // We have successfully parsed "20" into 20.
          // but we don't like even numbers...
          if two % 2 == 0 {
              Err(MyError("I don't add even numbers".to_owned())) // Result<u32, MyError>
          } else {
              Ok(one + two)
          }
      })
})
```

What if we want to parse two numbers and add them together?

```{.rust .scrollx}
parse_number("10")
  .and_then(|ten| {
      // We have successfully parsed "10" into 10.
      parse_number("20")
        .and_then(|twenty| {
          // We have successfully parsed "20" into 20.
          // What do we return here?
          todo!()
        })
  })
```

We could return a `Result` like before:

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
// pseudocode
// Given a Result<T, E>
F: T -> U // Convert success value to a U

Ok(t:T)   ->  F(t)  -> U // Return converted value in Ok, as a Result<U, E>
Err(e:E)            -> E // Return existing error as Result<U, E>
```

We can see that after we apply the function `F`, we still return a `Result`. This is why we can use `map` within an `and_then` call.

So when do we use `and_then` when chaining `Result`s and when do we use `map`?

If you need to make a decision about whether to fail or not, then use `and_then` because you
can return an `Ok` to succeed or an `Err` to fail. If you simply want to work on a the `Ok` side of a previous `Result`, then use `map`.

Lets try to perform a calculation on multiple numbers parsed from strings:

```{.rust .scrollx}
let numbers_1: Result<u32, ParseIntError> = add_numbers("10", "20", "30"); // Ok(60)
let numbers_2 = add_numbers("ten", "20", "30");    // Err(ParseIntError { kind: InvalidDigit })
let numbers_3 = add_numbers("10", "twenty", "30"); // Err(ParseIntError { kind: InvalidDigit })
let numbers_4 = add_numbers("10", "20", "thirty"); // Err(ParseIntError { kind: InvalidDigit })
```

Here's what `add_numbers` would look like:

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

This is similar what we did with two numbers parsed from `Result`. But as we can see this is quickly becoming hard to reason about.
Luckily Rust gives us a simpler way to do this.


### The question mark operator

Rust has the [question mark operator](https://doc.rust-lang.org/reference/expressions/operator-expr.html#the-question-mark-operator) (`?`) which allows you to simply
return an error or extract a success value. You can think of it as an `unwrap` on `Ok` with an immediate return on `Err` instead of panic-ing. Here's another the definition of `and_numbers` which uses the `?` operator:

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

We can see that the `add_numbers_2` function is easier to reason about than chaining together `and_then` and `map` calls in the `add_numbers` function. The `?` operator is supported for `Result` and `Option` types at the moment.

### and

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

This can be useful when you only want to know if something succeeded or failed instead of needing to work on its value.

Take creating a directory and then creating a file in that directory only if the directory creation succeeded.

We can create a directory with the `create_dir` function from the `std::fs` module:

```{.rust .scrollx}
fn create_dir<P: AsRef<Path>>(path: P) -> io::Result<()>
```

Notice how this function returns a `Result` with a `Unit` as the success value.

We can create a file with the `create` function on the `std::fs::File` struct:

```{.rust .scrollx}
fn create<P: AsRef<Path>>(path: P) -> io::Result<File>
```

If we use `and_then` to complete the example use case:

```{.rust .scrollx}
fn create_directory_and_then_file(dir_path: &Path, file_name: &str) -> io::Result<File> {
  create_dir(dir_path)
    .and_then(|_| { // We ignore the value from create_dir
      File::create(dir_path.join(file_name))
    }) // Result<File>
}
```

we can see that we have to ignore the previous success value in `and_then`. This is a little verbose and we can trim it down with `and`:

```{.rust .scrollx}
fn create_directory_and_file(dir_path: &Path, file_name: &str) -> io::Result<File> {
  create_dir(dir_path)
    .and(File::create(dir_path.join(file_name)))  // Result<File>
}
```

### or

If you wanted to try an alternative `Result` on `Err` and you didn't care about the error value, you could use `or`. `or` is defined as:

```{.rust .scrollx}
 pub fn or<F>(self, res: Result<T, F>) -> Result<T, F> {
   match self {
     Ok(v) => Ok(v),
     Err(_) => res,
   }
 }
```

From the above definition we can see that the value `res` is used only when there is an `Err` instance. If the `Result` is an `Ok` instance, its value
is returned.

in summary:

```{.rust .scrollx}
// pseudocode
// Given a Result<T, E>

Err(_:E) -> res:Result<T, F>  -> Result<T, F> // Notice that the `Err` type can change from `E` to `F`
Ok(t:T)  -> Ok(t)             -> Result<T, F> // `Ok` type is fixed: `T`
```

It's important to note that `res` dictates the final `Err` type returned from `or` and that the type inside the `Ok` constructor doesn't change. We'll see that come into play in the example below.

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

and a function to parse numbers and parse booleans:

```{.rust .scrollx}
fn parse_number(value: &str) -> Result<u32, ParseIntError> {
  u32::from_str(value)
}

fn parse_bool(value: &str) -> Result<bool, ParseBoolError> {
  bool::from_str(value)
}
```

One thing to note is that both functions return different error types in `Err`: `ParseIntError` and [ParseBoolError](https://doc.rust-lang.org/std/str/struct.ParseBoolError.html) respectively.

How would we combine these functions into parsing a string slice into a type of `MyResult`? Oh, and we also don't support converting a string that is all caps into `MyResult`. That would be an error.

`Note` that we don't need to align the error types here as mentioned before because the `Result` passed to `or` would change the final `Err` type as required.

Here's one way we could do it:

```{.rust .scrollx}
fn parse_my_result(value: &str) -> Result<MyResult, MyError> {
  parse_number(value) // Result<u32, ParseIntError>
    .map(|n| MyResult::N(n)) // Result<MyResult, ParseIntError>
    .or(
      parse_bool(value) // Result<u32, ParseBoolError>
        .map(|b| MyResult::B(b))
    ) // Result<MyResult, ParseBoolError>
    .or(
      if value.to_uppercase() == value {
          // We don't support full caps
          Err(MyError(format!("We don't support screaming case: {}", value))) // Result<MyResult, MyError>
       } else {
        Ok(MyResult::S(value.to_owned())) // Result<MyResult, MyError>
       }
    ) // Result<MyResult, MyError>
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
  let p1: Result<MyResult, ParseIntError> =
    parse_number(value)
      .map(|n| MyResult::N(n));

  let p2: Result<MyResult, ParseBoolError> =
    parse_bool(value)
      .map(|b| MyResult::B(b));

  let p3: Result<MyResult, MyError> =
    if value.to_uppercase() == value {
        // We don't support full caps
        Err(MyError(format!("We don't support screaming case: {}", value)))
     } else {
      Ok(MyResult::S(value.to_owned()))
     };

    let r1: Result<MyResult, ParseBoolError> = p1.or(p2); // p2's type wins
    let r2: Result<MyResult, MyError> = r1.or(p3); // p3's type wins

    r2
}
```

The function `res`, passed to `or` dictates the final `Err` type. Also when chaining multiple `or` calls, the final `res` block dictates the final `Result` type. In the case of `or` chaining, the `Ok` type is fixed but the `Err` type can vary!

### or_else

`or_else` is similar to `or` with the exception that you get access to the error type `E`:


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
// Given a Result<T, E>
Err(e:E) -> op(e)  -> Result<T, F> // `Err` type goes from `E` -> `F`
Ok(t:T)  -> Ok(t)  -> Result<T, F> // `Ok` type is fixed: `T`
```

This can be useful when you need access to the error to make a decision about the result to return or when you need to log the error.

For example, if you want to log the error before returning a fallback:

```{.rust .scrollx}
fn parse_number_somehow(value: &str) -> Result<u32, MyError> {
  parse_number(value)
    .or_else(|e| {
      eprintln!("Could not convert '{}' to a number: {}, defaulting to length", value, e);
      parse_by_length(value)
    })
}

fn parse_by_length(value: &str) -> Result<u32, MyError> {
  let length = value.len();
  if length <= u32::MAX as usize {
    Ok(length as u32)
  } else {
    Err(MyError("Your string is too long for u32".to_owned()))
  }
}

parse_number_somehow("number")
//Could not convert 'number' to a number: invalid digit found in string, defaulting to length
// Ok(6)
```

## Working with errors


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
