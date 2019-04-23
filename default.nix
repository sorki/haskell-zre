{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
let
    binary-strict = nixpkgs.haskell.packages.${compiler}.callPackage ./nix/binary-low-level.nix {};
    # needs 0.8
    zeromq4-haskell = nixpkgs.haskell.packages.${compiler}.callPackage ./nix/zeromq-haskell.nix {};
in
nixpkgs.haskell.packages.${compiler}.callPackage ./zre.nix { inherit binary-strict zeromq4-haskell; }
