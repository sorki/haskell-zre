{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./zre.nix { }
