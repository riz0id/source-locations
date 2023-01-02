{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, template-haskell
}:
mkDerivation {
  pname = "prim-ord";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-ord";
    sha256 = "0gnl9sqiicvf48s3xlx0l0zpngg0sxafznsxgimc79qrjdnbn831";
    rev = "e3c776e60196a26a557124a2403174fa4c94da06";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-ord";
  description = "TODO";
  license = lib.licenses.isc;
}
