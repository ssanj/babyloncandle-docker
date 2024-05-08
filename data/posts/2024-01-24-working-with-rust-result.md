---
title: Working With Rust Result
author: sanjiv sahayam
description: Working with Rust Result
tags: rust
---

Trying to learning how to use the Rust [Result](https://doc.rust-lang.org/std/result/enum.Result.html) type can be daunting. In this "Working with Rust Result" series of short posts, I hope to make that more approachable. This series is for beginners who are finding it difficult to understand what a `Result` is and how to use it.

The series is split into fourteen parts as listed below.

1. [What is a Result?](2024-01-24-working-with-rust-result-part-1.html) - ([Construction](2024-01-24-working-with-rust-result-part-1.html#construction))
1. [Extracting Values](2024-01-24-working-with-rust-result-part-2.html) - ([Pattern matching](2024-01-24-working-with-rust-result-part-2.html#pattern-matching), [map_or_else](2024-01-24-working-with-rust-result-part-2.html#map_or_else), [map_or](2024-01-24-working-with-rust-result-part-2.html#map_or))
1. [Extracting Values That Can Panic](2024-01-24-working-with-rust-result-part-3.html) - ([unwrap](2024-01-24-working-with-rust-result-part-3.html#unwrap), [expect](2024-01-24-working-with-rust-result-part-3.html#expect))
1. [Making Things Nicer with Fallbacks](2024-01-24-working-with-rust-result-part-4.html) - ([unwrap_or](2024-01-24-working-with-rust-result-part-4.html#unwrap_or), [unwrap_or_else](2024-01-24-working-with-rust-result-part-4.html#unwrap_or_else), [unwrap_or_default](2024-01-24-working-with-rust-result-part-4.html#unwrap_or_default))
1. [Transforming Values](2024-01-24-working-with-rust-result-part-5.html) - ([map](2024-01-24-working-with-rust-result-part-5.html#map))
1. [Combining Results](2024-01-24-working-with-rust-result-part-6.html) - ([and_then](2024-01-24-working-with-rust-result-part-6.html#and_then), [Aligning error types](2024-01-24-working-with-rust-result-part-6.html#aligning-error-types))
1. [Chaining with Map](2024-01-24-working-with-rust-result-part-7.html)
1. [Combining Results the Question Mark Operator](2024-01-24-working-with-rust-result-part-8.html) - ([The question mark operator](2024-01-24-working-with-rust-result-part-8.html#the-question-mark-operator), [Keep aligning those error values](2024-01-24-working-with-rust-result-part-8.html#keep-aligning-those-error-values))
1. [Combining Results Some More](2024-01-24-working-with-rust-result-part-9.html) - ([and](2024-01-24-working-with-rust-result-part-9.html#and), [or](2024-01-24-working-with-rust-result-part-9.html#or), [or_else](2024-01-24-working-with-rust-result-part-9.html#or_else))
1. [Working with Errors](2024-01-24-working-with-rust-result-part-10.html)  - ([map_err](2024-01-24-working-with-rust-result-part-10.html#map_err), [unwrap_err](2024-01-24-working-with-rust-result-part-10.html#unwrap_err), [expect_err](2024-01-24-working-with-rust-result-part-10.html#expect_err))
1. [Conversion to Option](2024-01-24-working-with-rust-result-part-11.html) - ([ok](2024-01-24-working-with-rust-result-part-11.html#ok), [err](2024-01-24-working-with-rust-result-part-11.html#err), [transpose](2024-01-24-working-with-rust-result-part-11.html#transpose))
1. [Value Tests](2024-01-24-working-with-rust-result-part-12.html) - ([is_ok](2024-01-24-working-with-rust-result-part-12.html#is_ok), [is_err](2024-01-24-working-with-rust-result-part-12.html#is_err), [is_ok_and](2024-01-24-working-with-rust-result-part-12.html#is_ok_and), [is_err_and](2024-01-24-working-with-rust-result-part-12.html#is_err_and))
1. [Asides](2024-01-24-working-with-rust-result-part-13.html) - ([Functions that return Result in std](2024-01-24-working-with-rust-result-part-13.html#functions-that-return-result-in-std), [Eager vs Laziness](2024-01-24-working-with-rust-result-part-13.html#eager-vs-laziness))
1. [Summary](2024-01-24-working-with-rust-result-part-14.html) - ([Cheatsheet](2024-01-24-working-with-rust-result-part-14.html#cheatsheet), [Feedback from the review lounge](2024-01-24-working-with-rust-result-part-14.html#feedback-from-the-review-lounge))

Now I know what you're thinking:

> Fourteen posts? You've got to be kidding me!

I know it's a lot of posts. I've tried to make each as small as possible with a single focus. I've added examples and some diagrams to make it more palatable.

Also don't feel the need to read the full series at one go. Read as much as you want or choose a topic you want to know more about or are currently struggling with and start there. Be sure to try some of the examples out and experiment with your own changes; That's the best way to learn.


Jump in at [What is a Result?](2024-01-24-working-with-rust-result-part-1.html)
