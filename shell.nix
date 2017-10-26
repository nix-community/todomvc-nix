let
  pkgs = import ./nix {};
in

pkgs.mkShell {
  mergeInputs = [pkgs.frontend pkgs.backend];
  buildInputs = [pkgs.stack pkgs.haskellPackages.ghc];
}
