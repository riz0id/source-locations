{ ghc ? "ghc922" }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        prim-int = self.callPackage ../pkgs/haskell/packages/prim-int.nix { };
      });
    };
  };
}