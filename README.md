# Lox Interpreters (Haskell + Odin)

Implementation of Lox from the book **Crafting Interpreters**  
<https://craftinginterpreters.com/>

## Projects

- **hlox** – jLox tree-walking interpreter implemented in **Haskell**
- **olox** – cLox VM interpreter implemented in **Odin**

## Progress

- ✅ hlox
- ❌ olox 

- Currently working on **Chapter 14**

## hlox (Haskell jLox)

A pure, persistent-structure implementation of jLox.

### Notes
* I am **Not** an expert haskell developer so there are probably lots of mistakes and non idiomatic code.

- Added `+=` operator
- Implemented proper `for` statement
- Added `break` and `continue`
- Added some additional builtin functions
- Uses persistent structures, so **no resolver class**

## olox (Odin cLox)

A VM interpreter modeled after cLox, implemented in **Odin**.

Implementation of Lox from the book Crafting Interpreters
