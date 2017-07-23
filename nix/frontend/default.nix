{ lib, mkYarnPackage, srcPath ? ../../frontend }:
mkYarnPackage {
  src = lib.sources.cleanSource srcPath;
  packageJson = srcPath + "/package.json";
  yarnLock = srcPath + "/yarn.lock";

  buildPhase = ''
    HOME=/tmp/home yarn build
  '';

  installPhase = ''
    mkdir -p $out/var
    cp -r dist/ $out/var/www
  '';
}
