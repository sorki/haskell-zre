{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./zre.nix { }
