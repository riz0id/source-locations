{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-int";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-int";
    sha256 = "1c00zgki1lsa248nj9jw3mm5hgjx21rjhc4szzmmxlpdcq2lag7z";
    rev = "df6cb5a82cc2fc6eba520fcbb22ee91c89ee2035";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  testHaskellDepends = [
    base ghc-prim hedgehog prim-bool prim-compat tasty tasty-hedgehog
  ];
  homepage = "https://github.com/riz0id/prim-int";
  description = "Facilities for working with unlifted integers";
  license = lib.licenses.isc;
}
