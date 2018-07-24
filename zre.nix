{ mkDerivation, async, attoparsec, base, binary, binary-strict
, bytestring, config-ini, containers, data-default, lifted-async
, monad-control, mtl, network, network-info, network-multicast
, optparse-applicative, process, random, repline, sockaddr, stdenv
, stm, text, time, transformers-base, uuid, zeromq4-haskell
}:
mkDerivation {
  pname = "zre";
  version = "0.1.0.1";
  sha256 = "0ddd7ppx8ivgrcvaj1d74vxjrgly8y5k5yr181dmi0z0jfkkkfx4";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary binary-strict bytestring config-ini
    containers data-default monad-control mtl network network-info
    network-multicast optparse-applicative process random sockaddr stm
    text time transformers-base uuid zeromq4-haskell
  ];
  executableHaskellDepends = [
    async base bytestring lifted-async monad-control mtl repline stm
    time
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/vpsfreecz/haskell-zre/";
  description = "ZRE protocol implementation";
  license = stdenv.lib.licenses.bsd3;
}
