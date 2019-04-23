{ mkDerivation, async, base, bytestring, containers, exceptions
, fetchgit, monad-control, QuickCheck, semigroups, stdenv, tasty
, tasty-hunit, tasty-quickcheck, transformers, transformers-base
, zeromq
}:
mkDerivation {
  pname = "zeromq4-haskell";
  version = "0.8.0";
  src = fetchgit {
    url = "https://gitlab.com/twittner/zeromq-haskell/";
    sha256 = "0cb75mc37c2l9mfn24057amsmgmk9sqzzqfqcb9sq8lw5nizbkv6";
    rev = "2649ab3ab27177776d6358e0b492203a05cb7ecf";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    async base bytestring containers exceptions monad-control
    semigroups transformers transformers-base
  ];
  libraryPkgconfigDepends = [ zeromq ];
  testHaskellDepends = [
    async base bytestring QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://gitlab.com/twittner/zeromq-haskell/";
  description = "Bindings to ZeroMQ 4.x";
  license = stdenv.lib.licenses.mit;
}
