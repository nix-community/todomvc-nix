{ lib, fetchFromGitHub }:

json:
  let
    data = lib.importJSON json;
  in
    fetchFromGitHub (builtins.removeAttrs data ["branch"])
