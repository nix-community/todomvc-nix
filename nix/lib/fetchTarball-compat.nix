# fetchTarball version that is compatible between all the versions of Nix
{ url, sha256 }@attrs:
if builtins.lessThan builtins.nixVersion "1.12" then
  fetchTarball { inherit url; }
else
  fetchTarball attrs
