{ system ? builtins.currentSystem }:
let
  flake = import ../. { inherit system; };
in
with flake.defaultNix.outputs.legacyPackages.${system}.nix;
{ inherit haskellBackend haskellFrontend rustBackend rustFrontend; }
