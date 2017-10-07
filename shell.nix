{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: [pkgs.aeson-pretty pkgs.aeson]);
in
pkgs.stdenv.mkDerivation rec {
  name = "led";
  buildInputs = [pkgs.tup ghc pkgs.sbcl pkgs.ecl pkgs.protobuf pkgs.lispPackages.cl-protobufs pkgs.lispPackages.swank pkgs.lispPackages.cl-cli];
}
