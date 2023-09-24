---
title: How to see Stdout When Running Tests in Rust
author: sanjiv sahayam
description: How to see the content of writing to stdout when running a test in Rust
tags: rust, cargo
comments: true
---

When coming to Rust from other languages, you expect certain frameworks to behave the same. One such instance, is that if you write to stdout through something like `println!` when running a test, you expect to see the output when the test is run.

By default Rust captures `stdout` and `stderr` when you run a test. Actually this applies only to tests that pass. If a test fails then both `stdout` and `stderr` are written out so you may have more information to fix the failing test.

> In hindsight I think this is a good default, because who hasn't see a test suite dumping out copious amounts of content to `stdout`? In the end this output is usually ignored and it just becomes noise. And you have to sit through just in case a test fails and you need to see all things the test was doing before it failed.

If you want to always write the contents of `stdout` and `stderr` then you can run tests with the `--nocapture` flag:

```terminal
cargo test -- --nocapture
```

From the [docs](https://doc.rust-lang.org/rustc/tests/index.html#--nocapture):

> ----nocapture
>
> Does not capture the stdout and stderr of the test, and allows tests to print to the console. Usually the output is captured, and only displayed if the test fails.

This is fine if you're are running a single test, but if you are running multiple tests in parallel then the output can be interleaved. This can make it hard to follow what's actually going on.

The recommended option, if you need all test output in order, is to use the `--show-output` flag:

```terminal
cargo test -- --show-output
```

The downside of the above is that all the output is written out *after* all the tests have completed instead of *while* the tests are running.

From the [docs](https://doc.rust-lang.org/rustc/tests/index.html#--show-output)

> ----show-output
>
> Displays the stdout and stderr of successful tests after all tests have run.
>
> Contrast this with ----nocapture which allows tests to print while they are running, which can cause interleaved output if there are multiple tests running in parallel, ----show-output ensures the output is contiguous, but requires waiting for all tests to finish.
