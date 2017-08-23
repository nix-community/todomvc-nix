{ lib, fetchFromGitHub, callPackage }:
let
  srcAttr = builtins.removeAttrs (lib.importJSON ./src.json) ["branch"];
  src = fetchFromGitHub srcAttr;
in
  callPackage src {}
