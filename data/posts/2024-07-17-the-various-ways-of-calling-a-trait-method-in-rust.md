---
title: The Various Ways Of Calling A Trait Method In Rust
author: sanjiv sahayam
description: the various ways of calling a trait method in rust
tags: rust
comments: true
---

There are many ways to invoke trait methods in Rust. This can get confusing quickly as some only work in specific situations.


## What is a trait method?

As the name implies a "trait method" is method that is defined on a trait. Let's take the following trait that converts a type to it's lowercase equivalent as an example:

```{.rust .scrollx}
trait Lower {
  fn lower(&self) -> Self;
}
```

Using the above trait, we can convert from an implementing type to a lowercase version of itself.

In summary:

```{.rust .scrollx}
implementation type: Self
trait: `Lower`
function: `lower`
function parameter type: none supplied (only `&self`)
```

Given, simple wrapper type `Identifier` over a `String`:

```{.rust .scrollx}
struct Identifier(String);
```

And an implementation for the `Lower` trait for `Identifier`:

impl Lower for Identifier {
  fn lower(&self) -> Self {
    Self(self.0.to_lowercase())
  }
}


How do we go about invoking the functions on the `Lower` trait on an implementation?

## The various ways

Here are the standard ways of invoking a trait methods:

1. Using an instance of the parameter type of the function on the trait
1. Using the type that implements the trait
1. Using the trait
1. Using the fully qualified implementation path to the function



## Another example

## What is a trait method?

As the name implies a "trait method" is method that is defined on a trait. Taking `From` as an example:

```{.rust .scrollx}
pub trait From<T>: Sized {
    /// Converts to this type from the input type.
    fn from(value: T) -> Self;
}
```

Using the above trait, we can convert from a `value` of some type `T`  to the implementing type (`Self`). The trait method
in this case is the `from` function. We can think of the `from` function as going from a type `T` -> `Self`.

In summary:

```{.rust .scrollx}
implementation type: Self (depends on what you use)
trait: `From`
function: from
function parameter type: `T` (`value` in the above example)
```

## The various ways

Here are the standard ways of invoking a trait methods:

1. Using an instance of the parameter type of the function on the trait
1. Using the type that implements the trait
1. Using the trait
1. Using the fully qualified implementation path to the function


We'll look at each in turn, but let's first look at a basic example implementation we can work through in each of the sections.

Given, simple wrapper type `Identifier` over a `String`:

```{.rust .scrollx}
struct Identifier(String);
```

And an implementation of the `From` trait for `Identifier`, that converts from a `String` to an `Identifier`:

```{.rust .scrollx}
impl From<String> for Identifier {
  fn from(value: String) -> Self {
    Identifier(value)
  }
}
```

In summary:

```{.rust .scrollx}
implementation type: `Identifier`
trait: `From<T>`, where `T` is `String`
function: from
param: `String`
```

### Using an instance of the parameter type of the function on the trait

The most ergonomic way to convert from a `String` to an `Identifier` would be to call a method on a `String` instance.

Given an `String` identifier:

```{.rust .scrollx}
let id_string = String::from("012BCE5");
```

let's convert it into an `Identifier` by calling the `from` function:

```{.rust .scrollx}
let id1: Identifier = id_string.from();
```

When we try the above we get a compilation error:

```{.terminal .scrollx}
   --> src/main.rs:137:37
    |
137 |     let id1: Identifier = id_string.from();
    |                           ----------^^^^--
    |                           |         |
    |                           |         this is an associated function, not a method
    |                           help: use associated function syntax instead: `String::from()`
    |
    = note: found the following associated functions; to be used as methods, functions must have a `self` parameter
```

The compiler is reminding us that we can't call `from` on an `String` instance because the `from` function does not take a
`self` parameter. The compiler also recommends that we use the "associated function syntax" to call the `from` function
on the type directly. We'll look at how to do that in [Using the type that implements the trait](#using-the-type-that-implements-the-trait), for now let's see if can use an instance
to do our conversion.

Rust implements the `Into` trait for each implementation of the `From` trait for free. The `Into` trait is defined as:

```{.rust .scrollx}
pub trait Into<T>: Sized {
  /// Converts this type into the (usually inferred) input type.
  fn into(self) -> T;
}
```

If you define `From` for a type, you get `Into` for that same type by the compiler with the following implementation:

```{.rust .scrollx}
impl<T, U> Into<U> for T
where
    U: From<T>,
{
    /// Calls `U::from(self)`.
    ///
    /// That is, this conversion is whatever the implementation of
    /// <code>[From]<T> for U</code> chooses to do.
    fn into(self) -> U {
        U::from(self)
    }
}
```

In summary:

```{.rust .scrollx}
implementation type: `T`
trait: Into<U>, where U is the implementer of `From<T>`
function: into
param: `T`
```

The implementation above, takes two type parameters: `T` and `U`. The `U` should have a `From` implementation for `T` and if it does, when calling the `into` method on an instance of `self`, invokes the conversion from `T` -> `U` through `U::from(self)`.

> Don't worry if this doesn't make sense just yet, but try to remember that if you implement the `From` trait you get a free `into` function that you can invoke on an instance of the parameter type of the `from` function.

Given that the `into` function takes a `self` reference, and in our example `self` is a `String`, we can call it on a `String` instance:

```{.rust .scrollx}
let id1: Identifier = id_string.into();
```

The above works! Yay!

If we leave off the `Identifier` type annotation on `id1`:

```{.rust .scrollx}
let id1 = id_string.into();
```

we get the following compilation error:

```{.terminal .scrollx}
error[E0283]: type annotations needed
   --> src/main.rs:138:9
    |
138 |     let id1 = id_string.into();
    |         ^^^                     ---- type must be known at this point
    |
note: multiple `impl`s satisfying `_: From<String>` found
   --> src/main.rs:84:1
    |
84  | impl From<String> for Identifier {
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    = note: and more `impl`s found in the following crates: `alloc`, `std`:
            - impl From<String> for Arc<str>;
            - impl From<String> for Box<(dyn std::error::Error + 'static)>;
            - impl From<String> for Box<(dyn std::error::Error + Send + Sync + 'static)>;
            - impl From<String> for Box<str>;
            - impl From<String> for OsString;
            - impl From<String> for PathBuf;
            - impl From<String> for Rc<str>;
            - impl From<String> for Vec<u8>;
    = note: required for `String` to implement `Into<_>`
help: consider giving `id1` an explicit type
    |
138 |     let id1: /* Type */ = id_string.into();
    |            ++++++++++++

For more information about this error, try `rustc --explain E0283`.
```

The compiler is letting us know that there are many `From` implementations for `String` and we need to give it a hint on
which one we want to use:

```{.terminal .scrollx}
help: consider giving `id1` an explicit type
    |
138 |     let id1: /* Type */ = id_string.into();
    |            ++++++++++++
```

###  Using the type that implements the trait

The format of calling a function on a type is:

```{.rust .scrollx}
<implementation type>::function(param)
```

In our case:

```{.rust .scrollx}
implementation type: `Identifier`
function: `from` (lives on the `From<T>` trait, where `T` is a `String`)
param: `String`
```

Running the conversion:

```{.rust .scrollx}
let id2 = Identifier::from(id_string)
```

We don't need to be explicit about the type of `id2` as the compiler can figure out what the type is.

### Using the trait

The format of calling a function on a trait is:

```{.rust .scrollx}
<trait>::function(param)
```

In our case:

```{.rust .scrollx}
trait: `From<T>` where `T` is a `String`
function: `from`
param: `String`
```


Running the conversion:

```{.rust .scrollx}
let id3: Identifier = From::from(id_string);
```

If we leave out the type of `id3` we get the following compilation error:


```{.terminal .scrollx}
error[E0790]: cannot call associated function on trait without specifying the corresponding `impl` type
   --> src/main.rs:144:15
    |
144 |     let id3 = From::from(id_string);
    |               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cannot call associated function of trait
    |
help: use a fully-qualified path to a specific available implementation
    |
144 |     let id3 = </* self type */ as From>::from(id_string);
    |               +++++++++++++++++++     +

For more information about this error, try `rustc --explain E0790`.
```

Given there are multiple `From` instances in scope, the compiler can't narrow it down until we give it more information.

The compilation error hints at using the full qualified path to the implementation (`type as trait`) format:

```{.terminal .scrollx}
help: use a fully-qualified path to a specific available implementation
    |
144 |     let id3 = </* self type */ as From>::from(id_string);
    |               +++++++++++++++++++     +
```

We'll look at that next.

### Using the fully qualified implementation path to the function

The format for using the fully qualified path to the implementation is:

```{.rust .scrollx}
<implementation type as trait>::function(param)
```

In our case:

```{.rust .scrollx}
implementation type: `Identifier`
trait: `From<T>`, where `T` is `String`
function: from
param: `String`
```

```{.rust .scrollx}
let id4 = <Identifier as From<String>>::from(id_string);
```

And in the above case as well, we can leave off the type of `id4` because we have been explicit as possible about which
implementation of `From` to use.
