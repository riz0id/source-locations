<div align="center">

# source-locations

</div>

This packages provides a type for representing source locations along with basic operations for creating and manipulating source locations. The `SrcLoc` type is (roughly) represented as a `Int` triple capturing a position (or offset) into a source file along with the line and column that position appears at in the source file.

- [Example](#example)
- [Documentation](#documentation)
- [Building](#building)
  - [Flag Reference](#flag-reference)
    - [The `llvm` Flag](#the-llvm-flag)
  - [Building via Nix](#building-via-nix) 

# Example 

TOOD

# Documentation

The `source-locations` package has full haddock coverage. To build the haddock documentation, run the following command: 

```bash
$ cabal v2-haddock
```

# Building 

TODO

- [Flag Reference](#flag-reference)
  - [The `llvm` Flag](#the-llvm-flag)
- [Building via Nix](#building-via-nix) 

## Flag Reference

Documentation for flags supported by [`source-locations.cabal`](./source-locations.cabal) that can be specified to configure how `source-locations` is built. 

### The `llvm` Flag

The `-fllvm` flag can be specified when building the `source-locations` package can to compile using GHC's LLVM backend. Compiling with LLVM can sometimes produce better much faster code in some cases, but take significantly longer to compile when compared to the native code generator. See the GHC User's Guide section on [the LLVM code generator](https://downloads.haskell.org/ghc/9.2.2/docs/html/users_guide/codegens.html?highlight=llvm#llvm-code-generator-fllvm).

```bash
$ cabal v2-build -fllvm
```

## Building via `Nix`

TODO
