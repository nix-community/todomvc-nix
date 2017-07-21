{ lib, mkYarnPackage, srcPath ? ../../frontend }:
mkYarnPackage {
  src = lib.sources.cleanSource srcPath;
  packageJson = srcPath + "/package.json";
  yarnLock = srcPath + "/yarn.lock";

  buildPhase = ''
    HOME=/tmp/home yarn build
  '';

  installPhase = ''
    cp -r dist/ $out
  '';
}
