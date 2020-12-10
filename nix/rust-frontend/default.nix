{ pkgs }:

let
  inherit (pkgs) stdenv lib;

  package = lib.importJSON ../../rust/frontend/package.json;

  # `wasm-pack` from https://github.com/rustwasm/wasm-pack/blob/master/npm/binary.js#L22-L29
  # use `binary-install` to download wasm-pack from release. However, since we're building
  # the project inside Nix, then we cannot have internet so downloading from the outside is
  # not possible. Hence, we create `runJS` that tries to replace `run.js` inside the wasm-pack
  # npm package. Please refer to `pkgConfig` below.
  runJS =  pkgs.writeScript "run.js" ''
    #!/usr/bin/env node
    const { join } = require("path");
    const { spawnSync } = require("child_process");
    const [, , ...args] = process.argv;

    const options = { cwd: process.cwd(), stdio: "inherit" };
    const result = spawnSync(join(process.cwd(),"wasm-pack"), args, options);

    if (result.error) {
      error(result.error);
    }
    process.exit(result.status);
  '';

  yarnPkg = pkgs.yarn2nix-moretea.mkYarnPackage rec {
    pname = "${package.name}";
    version = package.version;
    src = null;
    dontUnpack = true;
    packageJSON = ../../rust/frontend/package.json;
    yarnLock = ../../rust/frontend/yarn.lock;

    preConfigure = ''
      mkdir ${package.name}
      cd ${package.name}
      ln -s ${packageJSON} ./package.json
      ln -s ${yarnLock} ./yarn.lock
    '';

    yarnPreBuild = ''
      mkdir -p $HOME/.node-gyp/${pkgs.nodejs.version}
      echo 9 > $HOME/.node-gyp/${pkgs.nodejs.version}/installVersion
      ln -sfv ${pkgs.nodejs}/include $HOME/.node-gyp/${pkgs.nodejs.version}
    '';

    pkgConfig = {
      wasm-pack = {
        buildInputs = [ pkgs.wasm-pack ];
        postInstall = ''
          rm run.js
          cp ${pkgs.wasm-pack}/bin/wasm-pack .
          ls -lah
          cp ${runJS} run.js
        '';
      };
    };

    publishBinsFor = [
      "wasm-pack"
      "rollup"
    ];
  };
in
stdenv.mkDerivation {
  name = "${package.name}-${package.version}";

  src = lib.cleanSourceWith {
    filter = name: type:
      !(lib.hasSuffix ".css" name) &&
      !(lib.hasSuffix ".html" name) &&
      !(lib.hasSuffix ".log" name) &&
      !(lib.hasSuffix ".nix" name) &&
      !(lib.hasSuffix "node_modules" name)
    ;
    src = ../../rust/frontend;
  };

  buildInputs = [
    yarnPkg
    pkgs.yarn
    pkgs.wasm-pack
  ];

  patchPhase = ''
    cp -r ${yarnPkg}/libexec/${package.name}/node_modules .
  '';

  buildPhase = ''
    # Yarn writes cache directories etc to $HOME.
    export HOME=$PWD/yarn_home
    export PATH=${yarnPkg}/libexec/${package.name}/node_modules/.bin:$PATH
    cp ${pkgs.wasm-pack}/bin/wasm-pack .
    yarn build
  '';

  installPhase = ''
    mkdir -p $out
    cp js/* $out/
  '';
}
