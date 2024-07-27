{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-int";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-int";
    sha256 = "0kqvsay2gck8279jhvv8f195738g7i9v2qn6mgn7g4fydb8napcj";
    rev = "6f8a4f116c6fb5e9b528029a4d821042b0116bf0";
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
