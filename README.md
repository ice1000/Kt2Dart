# Kt2Dart

[![Build status](https://ci.appveyor.com/api/projects/status/38gy6t4offcp39jb?svg=true)](https://ci.appveyor.com/project/ice1000/kt2dart)

This is a transpiler which consumes a subset of Kotlin and convert it into Dart.

Currently it's completely in progress. But it can do some simple jobs right now.

## Limitations

I will not add any support for (and this tool will raise warnings as comments when these stuffs are detected):

+ **Extensions**
+ Labels (label break/continue, label return)
+ `import as`
+ **Anonymous Classes**

While these features are not fully supported:

+ Naming (this tool will NOT change the names you use, which means that names like `System.\`in\`` are transpiled into `System.in` directly)

Such (advanced) language features of Kotlin are too complex to transpile
(it needs too much analyzing while this is just a simple tool).

# Why Kt2Dart

Because I want to **Make [Flutter](https://flutter.io) Great Again**.

Flutter, written in Dart, is an awesome mobile developing framework. But Dart is **too** weak, for the dynamic typing and null-unsafety.

Kotlin is much more powerful.
If we can combine the **beautiful** flutter and the **powerful/safe** Kotlin, our world will be peaceful forever.

# Why Haskell

Yeah, as you see, I'm using Haskell in this project.
Because ~~Parser Combinators are easy and convenient~~ I want to practise my Haskell skills.

I'm a beginner to Haskell, so if you find any naive code, feel free to tell me (by issuing or mailing) (I'm using hlint).
