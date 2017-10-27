# fetchTarball version that is compatible between all the versions of Nix
{ url, sha256 }@attrs:
let
  inherit (builtins) lessThan nixVersion fetchTarball;
in
if lessThan nixVersion "1.12" then
  fetchTarball { inherit url; }
else
  fetchTarball attrs
