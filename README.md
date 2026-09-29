# Lox Interpreters (Haskell + C3)

Implementation of Lox from the book **Crafting Interpreters**  
<https://craftinginterpreters.com/>

## Projects

- **hlox** – jLox tree-walking interpreter implemented in **Haskell**
- **c3lox** – cLox byte code interpreter implemented in **C3**

## Progress

- ✅ hlox
- ❌ c3lox 

- Currently working on **Chapter 20**

## hlox (Haskell jLox)

A pure, persistent-structure implementation of jLox.

### Notes
- I am **Not** an expert haskell developer so there are probably lots of mistakes and non idiomatic code
- Cabal Project
- Added `+=` operator
- Implemented proper `for` statement
- Added `break` and `continue`
- Added some additional builtin functions
- Uses persistent structures, so **no resolver class**

## c3lox (C3 cLox)

A byte code interpreter modeled after cLox, implemented in **C3**.

### Notes
- Trying to use as idiomatic C3 code as I can, so it does diverge from clox. If you want a c3 version as c-like as possible then look at this repo https://github.com/worky68/c3lox
- Going to avoid using pointer arithmetic and data structures like linked lists.
- Data Structures that don't need to be gc will be allocated using temp allocator and cleaned up after interpreter leaves scope.

