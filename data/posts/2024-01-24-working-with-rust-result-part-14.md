---
title: Working With Rust Result - Summary - Part 14
author: sanjiv sahayam
description: Working with Rust Result - Summary of Result methods
tags: rust
comments: true
---

If you've made it this far, you're probably overwhelmed by all the different methods and their uses!
It helps to try and learn to use them one at a time; as and when needed. The following table summarises which method you would use under different circumstances.

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
      <td align="center">Get both values out of a `Result`</td>
      <td align="left">
        <ul>
          <li>`Patten matching`</li>
          <li>`map_or_else`</li>
          <li>`map_or`</li>
          <li>`unwrap` panics</li>
          <li>`unwrap_or`</li>
          <li>`unwrap_or_else`</li>
          <li>`unwrap_or_default`</li>
          <li>`expect` panics</li>
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
          <li>`unwrap` panics</li>
          <li>`expect` panics</li>
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
          <li>`unwrap_err` panics</li>
          <li>`expect_err` panics</li>
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

I hope this somewhat lengthy series helped you learn some more about Rust's `Result` type. If you found any errors or omissions or found this useful, please leave a comment.

## Feedback from the review lounge

### SirKastic23

A huge thanks to `SirKastic23` for giving me some sound feedback on Reddit:

<blockquote class="reddit-embed-bq" data-embed-showtitle="true" data-embed-theme="dark" data-embed-height="386"><a href="https://www.reddit.com/r/rust/comments/1ckbn6f/comment/l2n8wtf/">Comment</a><br> by<a href="https://www.reddit.com/user/ssanjs/">u/ssanjs</a> from discussion<a href="https://www.reddit.com/r/rust/comments/1ckbn6f/working_with_rust_result/"><no value=""></no></a><br> in<a href="https://www.reddit.com/r/rust/">rust</a></blockquote><script async="" src="https://embed.reddit.com/widgets.js" charset="UTF-8"></script>

I've made the updates to the sections marked "unsafe". I'll get to dark mode in the near future.

- Back to [TOC](2024-01-24-working-with-rust-result.html)
