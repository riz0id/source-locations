{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, prim-int, template-haskell
}:
mkDerivation {
  pname = "prim-char";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-char";
    sha256 = "16ici52g3a7bm6i9wqng5x6xhjh6zbkr4dsidqqr31gzx4gvy73g";
    rev = "99444178264c30e3c3a4168d3ea0315dc901490f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat prim-int template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-char";
  description = "Facilities for working with unboxed characters";
  license = lib.licenses.isc;
}
