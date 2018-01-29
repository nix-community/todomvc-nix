let
  pkgs = import ./nix {};
in

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
