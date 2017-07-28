# Kt2Dart

[![Build status](https://ci.appveyor.com/api/projects/status/38gy6t4offcp39jb?svg=true)](https://ci.appveyor.com/project/ice1000/kt2dart)

This is a transpiler which consumes a subset of Kotlin and convert it into Dart.

Currently it's completely in progress. But it can do some simple jobs right now:

```haskell
Prelude> :l Kt2Dart.hs
[1 of 3] Compiling Parsers          ( Parsers.hs, interpreted )
[2 of 3] Compiling OperatorP        ( OperatorP.hs, interpreted )
[3 of 3] Compiling Kt2Dart          ( Kt2Dart.hs, interpreted )
Ok, 3 modules loaded.
*Kt2Dart> parseCode kotlinStatement "a?.func(a.b + c.d, e ?: b)"
Left "a?.func((a.b+c.d),(e??b));"
*Kt2Dart> parseCode kotlinStatement "a?.func(a.b)"
Left "a?.func(a.b);"
*Kt2Dart> parseCode kotlinLambda "{ a -> return a; }"
Left "(a){return a;}"
*Kt2Dart> parseCode kotlinLambda "{ a, b, c -> println(b) ; shit(c + a); return a; }"
Left "(a,b,c){println(b);shit((c+a));return a;}"
*Kt2Dart> parseCode kotlinStatement "a in (a.b)"
Left "a.b.contains(a);"
*Kt2Dart> parseCode kotlinStatement "a in a.b"
Left "a.b.contains(a);"
*Kt2Dart> parseCode kotlinStatement "a !in a.b"
Left "!a.b.contains(a);"
*Kt2Dart> parseCode kotlinStatement "a is a.b"
Left "(a is a.b);"
*Kt2Dart> parseCode kotlinStatement  "run { a -> a in b }"
Left "run((a){b.contains(a)();});"
*Kt2Dart> parseCode kotlinStatement  "a.run() { a -> a in b }"
Left "a.run((a){b.contains(a)();});"
*Kt2Dart> parseCode kotlinStatement  "a.run(x) { a -> a in b }"
Left "a.run(x,(a){b.contains(a)();});"
```
