{ ghc ? "ghc922" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.source-locations