---
title: Working With Rust Result
author: sanjiv sahayam
description: working with rust result
tags: Rust
comments: true
---

Languages like [Haskell](https://www.haskell.org/) and [Scala](https://www.scala-lang.org/) have a type called [Either](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Either.html) to represent a value that maybe successful or in error. In Rust this type is called a [Result](https://doc.rust-lang.org/std/result/index.html) and is defined as:

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
   Ok(T), // All good
   Err(E), // Oh noes
}
```

Many of the `std` functions return `Result`s if the action you're trying to perform can fail. Here's
an example from the [Write](https://doc.rust-lang.org/std/io/trait.Write.html#method.write_all) trait in `std::io`:

```{.rust .scrollx}
trait Write {
  fn write_all(&mut self, buf: &[u8]) -> Result<()>
}
```

Using the `write_all` method to write the contents of the supplied buffer (`buf`) can fail with IO errors, or it could succeed in which case a Unit (`()`) value is returned.

Now hold on, `Result` should have two type variables and the example above clearly only has one. What's going on?

A frequent pattern used in Rust libraries is to create an alias for `Result` that wraps a particular error type. In the example above, `Result` is [aliased](https://doc.rust-lang.org/std/io/type.Result.html) as follows:

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

Let's look at how to constructor success and error values using `Result`. We mentioned that we use the `Ok` constructor for success values and the `Err` constructor
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

If we wanted to print calls to the above function, we could do something like this:

```{.rust .scrollx}
println!("{:?}", twenty_five(5));  //Err("5 is not 25!")
println!("{:?}", twenty_five(25)); //Ok(25)
println!("{:?}", twenty_five(35))  //Err("35 is not 25!")
```

We have used the debug syntax (`{:?}`) to display the value of the `Result` in the above example. What if we want to actually get the success or error value "out" of a `Result` instead of just printing it?

## Extracting values from a Result

### Pattern Matching

Since `Rust` supports [pattern matching](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html), a simple way to extract a value from a `Result` is to pattern match:

```{.rust .scrollx}
fn print_age(age_result: Result<u8, String>) {
  match age_result {
    Ok(age)    => println!("You are twenty five!"), // we could also do something with 'age' if we wanted.
    Err(error)  => println!("Imposter! {}", error),
  }
}

print_age(twenty_five(20)); //Imposter! 20 is not 25!
print_age(twenty_five(25)); //You are twenty five!
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
D: E -> U // Convert error value to a U
F: T -> U // Convert success value to a U
```

 `D` is used on the error value inside an `Err` instance and `F` is used on the success value inside an `Ok` instance. `map_or_else` has simply run a function on each data constructor (`Ok` and `Err`) to produce a result of the same type in all cases. This type of construct is also called a `fold` in some languages.

### map_or

Another function in the same family of functions is `map_or`:

```{.rust .scrollx}
pub fn map_or<U, F: FnOnce(T) -> U>(self, default: U, f: F) -> U {
    match self {
        Ok(t) => f(t),
        Err(_) => default, // We ignore the value and return an default
    }
}
```

In the above definition, a function `F` runs on the value inside the `Ok` instance and a default value is returned if it's an `Err` instance:

```{.rust .scrollx}
// pseudocode
D:   -> U // Return a U if in error
F: T -> U // Convert success value to a U
```

Notice that we completely ignore the value inside of the `Err` instance.

`map_or` differs from `map_or_else`, in that it only takes a single function `F` and a default value to return in the `Err` case. This can be useful if you don't care about what the error case was and simple want to return some default value. Maybe this function should be renamed to `map_or_default`.


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

We can see that the above definition returns the success type `T` under all conditions. But how can it return a success value `T` if it's an `Err` instance with a value type `E`?

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

But how do you do that? We've already seen some ways to do that with pattern matching and `map_or_else`. We can choose to do whatever we want to in each instance. There are other ways which we will look at next.

### Making things safer with defaults and fallbacks

One way we can `unwrap` a `Result` safely, is to provide a default value or function that returns a value of type `T` (The `Ok` type) when there is an `Err`.

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

We can see from the above definition that, we supply a default value of `T`. This default value will be used when there is an `Err`, the `Ok` value will be returned otherwise. This is very similar to [map_or](#map_or) but where we don't run a function on the success value.

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

This is also very similar to the `unwrap_or` where we supply a default value for the error case. In `unwrap_or_default` the default value is derived from the `Default` instance.

## Transforming values within a Result

What if you could run a function on the value within a `Result` and get a new `Result` back? Then you wouldn't have to do all the pesky unwrapping until when you actually needed the value.

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
F: T -> U // Convert success value to a U and return a Result<U, E>
```

In either case the resulting `Result`is converted from a `Result<T, E>` to a `Result<U, E>`. It's important to note that we stay within a `Result` after running the function `F`. Here's a simple example demonstrating this:

```{.rust .scrollx}
  let result_ok_1: Result<u32, String> = Ok(1);
  let result_ok_2: Result<u32, String> = result_ok_1.map(|n| n * 2); // Ok(2), multiplied by 2
  let result_ok_3: Result<String, String> = result_ok_2.map(|n| format!("age: {}", n)); // Ok("age: 2"), converted to a String

  let result_err_1: Result<u32, String> = Err("You have errors".to_owned());
  let result_err_2: Result<u32, String> = result_err_1.map(|n| n * 2); // Err("You have errors"), no change
  let result_err_3: Result<String, String> = result_err_2.map(|n| format!("age: {}", n)); // Err("You have errors"), no change
```


## Combining Results for fun and profit

`Results` get really useful when you can combine multiple of them to give you one final `Result`.

### and_then

This is really useful when you have multiple functions that return `Result`s and you want to know if all of them succeeded or any of them failed. `and_then` is defined as:

```{.rust .scrollx}
pub fn and_then<U, F: FnOnce(T) -> Result<U, E>>(self, op: F) -> Result<U, E> {
    match self {
        Ok(t) => op(t),
        Err(e) => Err(e),
    }
}
```

From the above definition, the function `F` is run on the success value with an `Ok` instance. This is very similar to [map](#map). The main difference is that the function `F` returns another `Result` instead of another type.

```{.rust .scrollx}
// pseudocode
// Give: Result<T, E>
F: T -> Result<U, E> // Converts a success value into another Result
E -> E // Returns the error if there is one, essentially short-circuiting the combination.
```

// TODO: Example

### and

`and` is similar to `and_then` except a default `Result` is used on an `Ok` instance and the error is returned on an `Err` instance:

```{.rust .scrollx}
pub fn and<U>(self, res: Result<U, E>) -> Result<U, E> {
    match self {
        Ok(_) => res,
        Err(e) => Err(e),
    }
}
```

This can be useful when you only want to know if something succeeded instead of needing to work on its value.

// TODO: Example

### or

```{.rust .scrollx}
  pub fn or<F>(self, res: Result<T, F>) -> Result<T, F> {
      match self {
          Ok(v) => Ok(v),
          Err(_) => res,
      }
  }
```

### or_else (lazy)


```{.rust .scrollx}
  pub fn or_else<F, O: FnOnce(E) -> Result<T, F>>(self, op: O) -> Result<T, F> {
      match self {
          Ok(t) => Ok(t),
          Err(e) => op(e),
      }
  }
```



## Working with errors

### map_err

```{.rust .scrollx}
  pub fn map_err<F, O: FnOnce(E) -> F>(self, op: O) -> Result<T, F> {
      match self {
          Ok(t) => Ok(t),
          Err(e) => Err(op(e)),
      }
  }
```
### unwrap_err

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


## Conversions to Option

### ok

```{.rust .scrollx}
  pub fn ok(self) -> Option<T> {
      match self {
          Ok(x) => Some(x),
          Err(_) => None,
      }
  }
```

### err

```{.rust .scrollx}
    pub fn err(self) -> Option<E> {
        match self {
            Ok(_) => None,
            Err(x) => Some(x),
        }
    }
```

### transpose

```{.rust .scrollx}
  impl<T, E> Result<Option<T>, E>

  pub const fn transpose(self) -> Option<Result<T, E>> {
      match self {
          Ok(Some(x)) => Some(Ok(x)),
          Ok(None) => None,
          Err(e) => Some(Err(e)),
      }
  }
```


## Value tests

### is_ok

```{.rust .scrollx}
  pub const fn is_ok(&self) -> bool {
      matches!(*self, Ok(_))
  }
```

### is_err

```{.rust .scrollx}
  pub const fn is_err(&self) -> bool {
      !self.is_ok()
  }
```

### is_ok_and

```{.rust .scrollx}
  pub fn is_ok_and(self, f: impl FnOnce(T) -> bool) -> bool {
      match self {
          Err(_) => false,
          Ok(x) => f(x),
      }
  }
```

### is_err_and

```{.rust .scrollx}
  pub fn is_err_and(self, f: impl FnOnce(E) -> bool) -> bool {
      match self {
          Ok(_) => false,
          Err(e) => f(e),
      }
  }
```


### References

### copied

for `impl<T, E> Result<&T, E>`

```{.rust .scrollx}
  pub fn copied(self) -> Result<T, E>
  where
      T: Copy,
  {
      self.map(|&t| t)
  }
```

for `impl<T, E> Result<&mut T, E>`

```{.rust .scrollx}
  pub fn copied(self) -> Result<T, E>
  where
      T: Copy,
  {
      self.map(|&mut t| t)
  }
```

### cloned

for `impl<T, E> Result<&T, E>`

```{.rust .scrollx}
  pub fn cloned(self) -> Result<T, E>
  where
      T: Clone,
  {
      self.map(|t| t.clone())
  }
```

for `impl<T, E> Result<&mut T, E>`

```{.rust .scrollx}
  pub fn cloned(self) -> Result<T, E>
  where
      T: Clone,
  {
      self.map(|t| t.clone())
  }
```

### as_ref

for `impl<T, E> Result<T, E>`

```{.rust .scrollx}
  pub const fn as_ref(&self) -> Result<&T, &E> {
      match *self {
          Ok(ref x) => Ok(x),
          Err(ref x) => Err(x),
      }
  }
```

### as_mut

for `impl<T, E> Result<T, E>`

```{.rust .scrollx}
  pub const fn as_mut(&mut self) -> Result<&mut T, &mut E> {
      match *self {
          Ok(ref mut x) => Ok(x),
          Err(ref mut x) => Err(x),
      }
  }
```

### as_deref

for `impl<T, E> Result<T, E>`

```{.rust .scrollx}
  pub fn as_deref(&self) -> Result<&T::Target, &E>
  where
      T: Deref,
  {
      self.as_ref().map(|t| t.deref())
  }
```

### iter

```{.rust .scrollx}
  pub fn iter(&self) -> Iter<'_, T> {
      Iter { inner: self.as_ref().ok() }
  }
```

### iter_mut

```{.rust .scrollx}
  pub fn iter_mut(&mut self) -> IterMut<'_, T> {
      IterMut { inner: self.as_mut().ok() }
  }
```
