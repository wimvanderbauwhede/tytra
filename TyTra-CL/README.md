# TyTraCL (“Coordination Language”)


## PREREQUISITES:

The essential prerequisites to build and cost programs with the TyTraCL compiler are:

- Working Haskell/Stack Installation
- Graphviz to explore the transformed AST Graph


## INSTALLATION:

  Clone the repository or otherwise download a snapshot:

    # `git clone` the repo

  Buld using stack:

    # stack build

## HOW TO USE:

  Call the compiler binary, passing in a file path and one or more transformation names to apply.

    $  stack exec bigbird-exe <input_file> [cleanup | autopar | inline]"
    
  Example input programs are provided in `./test/hllExmaples`






#### Haskell

The compiler is written in `Haskell`.

- [ghc >= 7.10 ](http://www.haskell.org) to compile
- Install the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
