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
    phases = ["nobuild"];

    buildInputs = mergeInputs' "buildInputs";
    nativeBuildInputs = mergeInputs' "nativeBuildInputs";
    propagatedBuildInputs = mergeInputs' "propagatedBuildInputs";

    nobuildPhase = ''
      echo "This derivation is not meant to be built, aborting";
      exit 1
    '';
  } // rest)
