# Tree-walking interpreter for the lox programming language

This is a repo containing a Haskell version of the Lox interpreter from the first part of the "Crafting Interpreters" book.

## Structure

All the modules so far are named after the Java classes used in the book. `Scanner` contains the tokeniser, `Parser` contains the parser, etc.

For tokenising, it just uses `String` everywhere because pattern matching makes this whole thing a breeze and we're unlikely to ever come across any source files large enough that switching to `Text` would be worth it.

The parser is a fairly straightforward `Parsec` parser, with the particularity that it operates on tokens directly rather than on characters as most tutorials do. This is because I already had a tokeniser module from the previous chapter.

The interpreter is implemented with the following types:
```
type Interpreter = ExceptT InterpreterError (StateT InterpreterState IO)

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either InterpreterError a, InterpreterState)
runInterpreter st i = runStateT (runExceptT i) st
```

This monad stack was chosen because:
- `ExceptT` allows is to throw exceptions from anywhere without having to wrap/unwrap Eithers all the time. This also matches the book, which depends on (Java) exceptions to bubble up errors.
- `StateT` to keep state about variable bindings etc.
- `IO` as base monad is required because the `PRINT` method is baked right into the language.

The resolver uses much the same structure as the interpreter, but with its own types. Unlike the book where the resolver directly updates maps inside the interpreter, the map with locals get returned from the resolver and fed into the interpreter.

## Currently working on

I now have most things working including classes with attributes and method calls, next up will be making the `this` keyword work inside classes.
