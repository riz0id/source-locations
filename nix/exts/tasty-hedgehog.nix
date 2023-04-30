{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (final: prev: {
        tasty-hedgehog = 
          if ghc == "ghc942"
            then final.callPackage ../pkgs/tasty-hedgehog.nix { }
            else prev.tasty-hedgehog;
      });
    };
  };
}