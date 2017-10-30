{ lib, stdenv }:

/* A special kind of derivation only meant to be consumed by nix-shell.

  It can take a list of derivation in the "mergeInputs" attribute. These derivation gets their buildInputs merged.
*/
{ mergeInputs ? [], buildInputs ? [], nativeBuildInputs ? [], propagatedBuildInputs ? [], ...}@attrs:
let
  mergeInputs' = name:
    let
      op = item: sum: sum ++ item."${name}" or [];
      nul = [];
      list = [attrs] ++ mergeInputs;
    in
      lib.foldr op nul list;

  rest =
    builtins.removeAttrs
      attrs
      ["mergeInputs" "buildInputs" "nativeBuildInputs" "propagatedBuildInputs"];
in
  stdenv.mkDerivation ({
    name = "nix-shell";
    phases = ["nobuildPhase"];

    buildInputs = mergeInputs' "buildInputs";
    nativeBuildInputs = mergeInputs' "nativeBuildInputs";
    propagatedBuildInputs = mergeInputs' "propagatedBuildInputs";

    nobuildPhase = ''
      echo
      echo "This derivation is only meant to be used in nix-shell, aborting";
      echo
      exit 1
    '';
  } // rest)
