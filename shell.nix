let
  pkgs = import ./nix {};
in

with pkgs;

mkShell {
  mergeInputs = [
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
