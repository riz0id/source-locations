{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.source-locations.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [ 

  ];
})
