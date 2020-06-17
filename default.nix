{ nixpkgs ? import <nixpkgs> {}}:
nixpkgs.haskell.lib.buildFromSdist
  (nixpkgs.haskellPackages.callCabal2nix "zre" ./. { })
