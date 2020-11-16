{ pkgs }:

with pkgs;

mkDevShell {
  name = "todomvc-nix";
  motd = "otherthing";
  commands = [
    {
      name = "pginit";
      help = "init psql service";
      command = "${todomvc.nix.database.pgutil.init_pg} || echo '''PG init failed''' ";
    }
    {
      name = "pgstart";
      help = "start psql service";
      command = "${todomvc.nix.database.pgutil.start_pg} || echo '''PG start failed''' ";
    }
    {
      name = "pgstop";
      help = "stop psql service";
      command = "${todomvc.nix.database.pgutil.stop_pg} || echo '''PG stop failed''' ";
    }
    {
      name = "migrate";
      help = "migrate database using sqitch";
      command = "${todomvc.nix.database.migrate}/bin/sqitch deploy || echo '''Migrate database failed''' ";
    }
    {
      name = "deletedb";
      help = "delete database using sqitch";
      command = "${todomvc.nix.database.migrate}/bin/sqitch revert || echo '''Migrate database failed''' ";
    }
    {
      name = "develop-miso";
      help = "develop miso app";
      command = ''
        ${todomvc.todoHaskellMisoDev.haskell.packages.ghc865.ghcid}/bin/ghcid -c \
          '${todomvc.todoHaskellMisoDev.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl todo-miso --write-ghc-environment-files never' \
          -W -s ':load Main' -T ':main'
      '';
    }
  ];

  bash = {
    extra = ''
      unset GOPATH GOROOT
      export LD_INCLUDE_PATH="$DEVSHELL_DIR/include"
      export LD_LIB_PATH="$DEVSHELL_DIR/lib"
    '';
    interactive = ''
    '';
  };

  env = {
    DATABASE_URL="postgresql://todomvc_dbuser:todomvc_dbpass@localhost:5432/todomvc_db";
    PGHOST="localhost";
    PGPORT="5432";
    PGDATABASE="todomvc_db";
    PGUSER="todomvc_dbuser";
    PGPASSWORD="todomvc_dbpass";
    GO111MODULE="on";
  };

  packages = [
    # project executable
    # Haskell
    # TODO: Make reflex/refex-dom project works
    # todomvc.todoHaskellObelisk.command

    # todomvc.nix.haskellMiso.dev.todo-miso
    todomvc.reflexDev.ghc.todo-haskell
    todomvc.reflexDev.ghc.todo-common

    # todomvc.reflexDev.ghc.frontend
    todomvc.todoHaskellMisoDev.haskell.packages.ghc865.cabal-install
    (todomvc.todoHaskellMisoDev.haskell.packages.ghc865.ghcWithPackages (p: with p; [
        jsaddle jsaddle-warp transformers warp websockets todo-common servant-jsaddle miso-jsaddle
      ])
    )
    # (todomvc.misoDev.ghcWithPackages (p: with p; [
    #     jsaddle jsaddle-warp transformers warp websockets todo-common
    #   ])
    # )
    # todomvc.todoHaskellMisoDev.ghcid
    # todomvc.nix.haskellMiso.prod.todo-miso-js

    ## haskell-nix
    # TODO: Make haskell.nix works in this project.
    # todomvc.nix.haskellNixBackend.todo-haskell.components.exes.todo-haskell

    ## haskellPackages
    ## haskell-nix
    # TODO: Make haskell.nix works in this project.
    #todomvc.nix.myHaskellNixPackages.hsPkgs

    # build tools
    ### Rust
	todomvc.nix.rust

    ### haskell tools
    ### haskell-nix
    # todomvc.nix.pkgs.myHaskellNixPackages.ghc
    # todomvc.nix.pkgs.myHaskellNixPackages.cabal-install
    # todomvc.nix.pkgs.myHaskellNixPackages.stack
    ### haskellPackages
    # (todomvc.todoHaskellPackages.ghcWithPackages
    #   (p: with p; [
    #     aeson aeson-pretty http-types todo-common warp zlib lens polysemy text
    #     unliftio wai wai-logger wai-extra warp servant servant-server postgresql-simple
    #   ])
    # )
    # todomvc.todoHaskellPackages.cabal-install
    # todomvc.todoHaskellPackages.stack
    # todomvc.todoHaskellPackages.haskell-language-server
    # todomvc.todoHaskellPackages.ghcid
    # todomvc.todoHaskellPackages.hlint
    # todomvc.todoHaskellPackages.ormolu
    ### Go
    go
    gopls
    gopkgs
    golint
    reflex
    gocode
    go-outline

    ### Others
    binutils
    pkgconfig
    openssl
    gcc
    glibc
    zlib.dev
    ncurses
    icu.dev
    gmp.dev

    # backend
    # todomvc.nix.backend

    # frontend
    nodejs-12_x
    yarn
    # yarn2nix
    # wasm-pack

    # database
    postgresql
    moreutils
  ];
}
