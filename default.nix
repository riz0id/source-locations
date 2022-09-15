{ ghc ? "ghc902" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    fourmolu
    haskell-language-server
    hlint
    source-locations;
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;

}

