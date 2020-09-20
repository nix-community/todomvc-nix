{ lib, mkYarnPackage, linkNodeModulesHook, srcPath ? ../../frontend }:
let
  filterNodeModules = name: type: name == "node_modules";

  rules = lib.path.composeFilters filterNodeModules lib.path.cleanSourceFilter;
in
mkYarnPackage {
  src = lib.path.filterOut rules srcPath;

  buildPhase = ''
    HOME=/tmp/home yarn build
  '';

  installPhase = ''
    mkdir -p $out/var
    cp -r dist/ $out/var/www
  '';

#   shellHook = linkNodeModulesHook;
}
