{ mkDerivation, async, attoparsec, base, binary, binary-strict
, bytestring, containers, mtl, network, network-info
, network-multicast, process, random, sockaddr, stdenv, stm, time
, uuid, zeromq4-haskell, lifted-async
}:
mkDerivation {
  pname = "zre";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary binary-strict bytestring containers
    mtl network network-info network-multicast process random sockaddr
    stm time uuid zeromq4-haskell lifted-async
  ];
  executableHaskellDepends = [ base bytestring ];
  testHaskellDepends = [ base ];
  description = "ZRE protocol implementation";
  license = stdenv.lib.licenses.bsd3;
}
