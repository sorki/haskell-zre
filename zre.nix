{ mkDerivation, async, attoparsec, base, binary, binary-strict
, bytestring, containers, lifted-async, monad-control, mtl, network
, network-info, network-multicast, optparse-applicative, process
, random, sockaddr, stdenv, stm, time, transformers-base, uuid
, zeromq4-haskell
}:
mkDerivation {
  pname = "zre";
  version = "0.1.0.0";
  sha256 = "11lnz7pxmqz39xjqjh1kkgywv0jg81yzi2hrp2ibaw2nslf65xzl";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary binary-strict bytestring containers
    monad-control mtl network network-info network-multicast
    optparse-applicative process random sockaddr stm time
    transformers-base uuid zeromq4-haskell
  ];
  executableHaskellDepends = [
    async base bytestring lifted-async monad-control mtl stm time
  ];
  testHaskellDepends = [ base ];
  description = "ZRE protocol implementation";
  license = stdenv.lib.licenses.bsd3;
}
