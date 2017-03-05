{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, pinNixDown ? false}:
let
  # Pin nixpkgs version down by default, but allow building with another version
  nixpkgs = if !pinNixDown then pkgs else import (pkgs.fetchgit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "af0fec6d0a3e28c815e38296f3758e7d0916eba9";
    sha256 = "0knbmva5bmilhz4w3xi55dg22m7g44viawxa5n5x228av3bcmy5i";
  }) {};

  drv = nixpkgs.haskell.packages.ghcjsHEAD.callPackage ./. {};
in
drv.env
