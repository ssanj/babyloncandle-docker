---
title: Package Control Is Missing On Sublime Text 4 On macOSX
author: sanjiv sahayam
description: Package Control is missing on Sublime Text 4 on macOSX
tags: sublime, macosx
comments: true
---

We use [Package Control](https://packagecontrol.io/installation) to install most packages we need with [Sublime Text](https://www.sublimetext.com). While there are other ways this is
the most convenient and provides access to the packages listed on [packagecontrol.io](https://packagecontrol.io).

Recently when I tried to install a new package and launched the Command Palette with `CMD`+`SHIFT`+`P`, I was surprised
to find that I couldn't find the Package Control commands.

I opened the console panel with `^`+``` ` ```, to look for any errors. I found an error related to the ctypes not loading:

```{.terminal .scrollx}
Traceback (most recent call last):
  File "../Library/Application Support/Sublime Text/Installed Packages/Package Control.sublime-package/package_control/deps/oscrypto/_openssl/_libcrypto_ctypes.py", line 305, in <module>
  File "./python3.3/ctypes/__init__.py", line 366, in __getattr__
  File "./python3.3/ctypes/__init__.py", line 371, in __getitem__
AttributeError: dlsym(0x7f876fc44440, EVP_PKEY_size): symbol not found
```

Cryptic.

My current setup is:


|Software| Version|
|-|-|
|macOS version| `Ventura 13.4.1`|
|Sublime Text version| `Build 4152`|

After a bit of digging around on [Sublime Text Issues](https://github.com/sublimehq/sublime_text/issues) I came across [Package Control Commands Missing](https://github.com/sublimehq/sublime_text/issues/6037) issue which linked to [\[ST3/4\] Package Control not working on macOS](https://github.com/wbond/package_control/issues/1612) issue on Package Control.


As far as I can tell the issue was related to an OpenSSL version mismatch. While Sublime Text ships with its own
OpenSSL library, Package Control relies on the OpenSSL version provided by the OS - macOS in this case.


The [workaround](https://github.com/wbond/package_control/issues/1612#issuecomment-1643609833) that worked for me was:

1. [Download](https://github.com/wbond/package_control/releases/download/4.0.0-beta2/Package.Control.sublime-package) the latest release (beta) of Package Control
2. Close Sublime Text
3. Open a terminal and go to your `Installed Packages` folder for Sublime Text. This can be found at `/Users/YOUR_USER/Library/Application Support/Sublime Text/Installed Packages`. You can also use `Sublime Text` > `Settings...` > `Browse Packages...` from the menu and go back one up directory to locate the `Installed Packages` folder.
4. Backup your `Package Control.sublime-package` which should be found under the `Installed Packages` folder.
5. Copy the `Package.Control.sublime-package` you downloaded in step one to the `Installed Packages` folder. Rename it to `Package Control.sublime-package`. Notice how you renamed a `Package.Control` to a `Package Control`.
6. Restart Sublime Text


Now hopefully that fixes your Package Control issues. If not check back on the linked issues for other resolutions.
