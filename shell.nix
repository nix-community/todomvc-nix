{ pkgs ? import <nixpkgs> {} }:

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
    # {
    #   name = "todo-hs-backend";
    #   help = "run todomvc from haskellPackages";
    #   command = "${todomvc.nix.pkgs.myHaskellPackages.todomvc-haskell}/bin/todomvc-haskell";
    # }
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
    ## haskellPackages
    ## haskell-nix
    # todomvc.nix.pkgs.myHaskellNixPackages.components.exes.todomvc-haskellNix

    # build tools
    ### Rust
	todomvc.nix.rust

    ### haskell tools
    (todomvc.nix.myHaskellPackages.ghcWithPackages(p: with p; [ zlib ]))
    todomvc.nix.myHaskellPackages.cabal-install
    todomvc.nix.myHaskellPackages.stack
    todomvc.nix.myHaskellPackages.haskell-language-server
    todomvc.nix.myHaskellPackages.hlint
    todomvc.nix.myHaskellPackages.ormolu

    ### haskell-nix
    # todomvc.nix.pkgs.myHaskellNixPackages.ghc
    # todomvc.nix.pkgs.myHaskellNixPackages.cabal-install
    # todomvc.nix.pkgs.myHaskellNixPackages.stack

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
    zlib.dev
    glibc

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
