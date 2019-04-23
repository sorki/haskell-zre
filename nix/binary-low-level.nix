{ mkDerivation, array, base, bytestring, containers, fetchgit, mtl
, stdenv
}:
mkDerivation {
  pname = "binary-strict";
  version = "0.4.8.4";
  src = fetchgit {
    url = "https://github.com/idontgetoutmuch/binary-low-level";
    sha256 = "059wbw68hah7zsp893xq4fmczsg0fyd22bfpi4ix3k1rjvr0gpbr";
    rev = "085211fa1ff8242947d84c44ec98a026e566c212";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ array base bytestring containers mtl ];
  homepage = "https://github.com/idontgetoutmuch/binary-low-level";
  description = "Binary deserialisation using strict ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}
