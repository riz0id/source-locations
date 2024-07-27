{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "07dcq67aa0ln8f2l39631hm06v231wr5aazwpsv0kn933djrhfj0";
    rev = "049a822782e523e996ccaa5c488c5abcf0ac810c";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ghc-prim prim-compat template-haskell
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-bool";
  description = "Unboxed booleans";
  license = lib.licenses.isc;
  mainProgram = "example";
}