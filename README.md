# Kt2Dart

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
```
