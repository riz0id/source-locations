{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, template-haskell
}:
mkDerivation {
  pname = "prim-ord";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-ord";
    sha256 = "1pb4pyj1sx5y1a0fagz0dc46mr26warx19iv0xjj6773rwq6v9yx";
    rev = "921f86fe72d3d410a7dc7588469bcda40aa0b125";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-ord";
  description = "TODO";
  license = lib.licenses.isc;
}
