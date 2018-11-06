# hse-parsing
A simple recursive descent parser. Written for the formal languages course in HSE. The language is described with the following grammar:

```
S -> Expr | \epsilon

Expr -> Ident = Expr
      | Term ((+ | *) Expr)?

Term -> Term2 ((* | /) Term2)?

Term2 -> Factor (^ Factor)?

Factor -> - Factor
        | Ident 
        | Num 
        | '(' Expr ')'

Ident -> 'a' | 'b' | ... | 'z'

Num -> '0' | '1' | ... | '9'
```

Running the build script `build.sh` generates an executable `Main`. `Main` parses several inputs specified and terminates.

To run parser on your input, load `Main` into the interpreter `ghci` and execute `parse <input>` or modify `Main.hs`.

This should work on any version of the haskell compiler, but has only been tested on `ghc 8.4.2`.

