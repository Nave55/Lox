# Lox Interpreters (Haskell + C3)

Implementation of Lox from the book **Crafting Interpreters**  
<https://craftinginterpreters.com/>

## Projects

- **hlox** – jLox tree-walking interpreter implemented in **Haskell**
- **c3lox** – cLox byte code interpreter implemented in **Odin**

## Progress

- ✅ hlox
- ❌ c3lox 

- Currently working on **Chapter 14**

## hlox (Haskell jLox)

A pure, persistent-structure implementation of jLox.

### Notes
- I am **Not** an expert haskell developer so there are probably lots of mistakes and non idiomatic code
- Cabal Project
- Just used Strict Map instead of HashMap
- Added `+=` operator
- Implemented proper `for` statement
- Added `break` and `continue`
- Added some additional builtin functions
- Uses persistent structures, so **no resolver class**

## c3lox (C3 cLox)

A VM interpreter modeled after cLox, implemented in **C3**.

Implementation of Lox from the book Crafting Interpreters
