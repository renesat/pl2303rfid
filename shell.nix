{ pkgs ? import <nixpkgs> { } }:

with pkgs;
haskell.lib.buildStackProject {
  inherit ghc;
  name = "pl2303";
  buildInputs = [
  ];
}
