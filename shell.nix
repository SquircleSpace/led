{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: [pkgs.aeson-pretty pkgs.aeson]);
in
pkgs.stdenv.mkDerivation rec {
  name = "led";
  buildInputs = [pkgs.tup ghc];
}
