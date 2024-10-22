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
- `ExceptT` allows us to throw exceptions from anywhere without having to wrap/unwrap Eithers all the time. This also matches the book, which depends on (Java) exceptions to bubble up errors and return early from functions.
- `StateT` to keep state about variable bindings etc.
- `IO` as base monad is required because the `PRINT` method is baked right into the language.

The resolver uses much the same structure as the interpreter, but with its own types. Unlike the book where the resolver directly updates maps inside the interpreter, the map with locals get returned from the resolver and fed into the interpreter.

I tried to keep the code as "Haskelly" as possible, defaulting to recursion and immutable datatypes wherever possible. The two main deviations are:
- The fields of a LoxInstance are kept in an `IORef (M.Map String RuntimeValue)`, to make them mutable. This is to simplify handling of long chains in Set expressions like `a.b.c.d = 5;`. If we were strict about using only immutable data structures, we would need to update `a`, `b`, `c` and `d` just to set a field in `d`. Due to the way instance lookups work, by the time we find the `LoxInstance` associated with `d`, the lookups for `a`, `b` and `c` are already out of scope. This way also stays closer to the book.
- Similarly the variable bindings in an Environment are kept in a `IORef (M.Map String RuntimeValue)`. In this case it is because the existence of closures messes up the nice linear chain of nested environments and makes it a tree instead. If we'd use immutable environments and update all "higher" environments recursively when a variable is updated, then the change to the variable would not be visible in the closures made when creating a function. Making it an `IORef` makes sure the changes are visible in all the environments.

## Currently working on

Nothing! This repo is pretty much complete as I've implemented (almost) everything in the book. The main thing lacking is having the parser report every error instead of bailing out at the first sign of trouble. This is because I wanted to make the parser using parser combinators and I couldn't figure out how to implement recovery in my setup.
