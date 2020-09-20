{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  inputsFrom = [
    frontend
    backend
  ];

  nativeBuildInputs = [
    git
    stack
  ];

  buildInputs = [
    haskellPackages.ghc
  ];
}
