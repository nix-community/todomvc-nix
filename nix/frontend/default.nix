{ lib, mkYarnPackage, linkNodeModulesHook, srcPath ? ../../frontend }:
mkYarnPackage {
  src = lib.sources.cleanSource srcPath;

  buildPhase = ''
    HOME=/tmp/home yarn build
  '';

  installPhase = ''
    mkdir -p $out/var
    cp -r dist/ $out/var/www
  '';

  shellHook = linkNodeModulesHook;
}
