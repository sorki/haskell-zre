{ mkDerivation, async, attoparsec, base, binary, bytestring
, config-ini, containers, data-default, directory, fetchgit
, filepath, lifted-async, monad-control, mtl, network, network-info
, network-multicast, optparse-applicative, process, QuickCheck
, quickcheck-instances, random, repline, sockaddr, stdenv, stm
, text, time, transformers-base, uuid, zeromq4-haskell
}:
mkDerivation {
  pname = "zre";
  version = "0.1.0.2";
  src = fetchgit {
    url = "https://github.com/sorki/haskell-zre";
    sha256 = "1s17k2h8izs9pkifavz0x4jzvmm6x4cinxix141cxhjnxkrwlf18";
    rev = "af18db0e59c438594fb7c772d1e3f990880db974";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary bytestring config-ini containers
    data-default directory filepath monad-control mtl network
    network-info network-multicast optparse-applicative process random
    sockaddr stm text time transformers-base uuid zeromq4-haskell
  ];
  executableHaskellDepends = [
    async base bytestring lifted-async monad-control mtl repline stm
    time
  ];
  testHaskellDepends = [
    base bytestring QuickCheck quickcheck-instances uuid
  ];
  homepage = "https://github.com/sorki/haskell-zre/";
  description = "ZRE protocol implementation";
  license = stdenv.lib.licenses.bsd3;
}
