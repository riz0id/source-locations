{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, prim-int, template-haskell
}:
mkDerivation {
  pname = "prim-char";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-char";
    sha256 = "0qwb1ks6qxzk91sv89agqq38y5dry031dv3r1a6ipbpc1bgc3xfb";
    rev = "c3b4228c11c6571c253463ebc2bb3a623a5408ca";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat prim-int template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-char";
  description = "Facilities for working with unboxed characters";
  license = lib.licenses.isc;
}
