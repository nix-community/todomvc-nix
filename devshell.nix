{ pkgs }:

with pkgs;

# Configure your development environment.
#
# Documentation: https://github.com/numtide/devshell
mkDevShell {
  name = "todomvc-nix";
  commands = [
    {
      name = "pginit";
      help = "init psql service";
      category = "database";
      command = "${todomvc.nix.database.pgutil.init_pg} || echo '''PG init failed''' ";
    }
    {
      name = "pgstart";
      help = "start psql service";
      category = "database";
      command = "${todomvc.nix.database.pgutil.start_pg} || echo '''PG start failed''' ";
    }
    {
      name = "pgstop";
      help = "stop psql service";
      category = "database";
      command = "${todomvc.nix.database.pgutil.stop_pg} || echo '''PG stop failed''' ";
    }
    {
      name = "migrate";
      help = "migrate database using sqitch";
      category = "database";
      command = "${todomvc.nix.database.migrate}/bin/sqitch deploy || echo '''Migrate database failed''' ";
    }
    {
      name = "deletedb";
      help = "delete database using sqitch";
      category = "database";
      command = "${todomvc.nix.database.migrate}/bin/sqitch revert || echo '''Migrate database failed''' ";
    }
    {
      name = "nixpkgs-fmt";
      help = "use this to format the Nix code";
      category = "fmt";
      package = "nixpkgs-fmt";
    }
  ];

  bash = {
    extra = ''
      export LD_INCLUDE_PATH="$DEVSHELL_DIR/include"
      export LD_LIB_PATH="$DEVSHELL_DIR/lib"
    '';
    interactive = '''';
  };

  env = {
    DATABASE_URL = "postgresql://todomvc_dbuser:todomvc_dbpass@localhost:5432/todomvc_db";
    PGHOST = "localhost";
    PGPORT = "5432";
    PGDATABASE = "todomvc_db";
    PGUSER = "todomvc_dbuser";
    PGPASSWORD = "todomvc_dbpass";
  };

  packages = [
    # Haskell

    # todomvc.reflexDev.ghc.frontend
    todomvc.todoHaskellMisoDev.haskell.packages.ghc865.cabal-install
    (todomvc.todoHaskellMisoDev.haskell.packages.ghc865.ghcWithPackages (p: with p; [
      http-client
      http-proxy
      jsaddle
      jsaddle-warp
      lens
      miso-jsaddle
      mtl
      servant-jsaddle
      text
      todo-common
      transformers
      warp
      websockets
    ])
    )

    # build tools
    ## Rust
    todomvc.nix.rust

    ## haskell tools
    ### haskellPackages
    # todomvc.nix.haskellBackend
    # (todomvc.todoHaskellPackages.ghcWithPackages
    #   (p: with p; [
    #     aeson aeson-pretty http-types todo-common warp zlib lens polysemy text
    #     unliftio wai wai-logger wai-extra warp servant servant-server postgresql-simple
    #   ])
    # )
    # todomvc.todoHaskellPackages.cabal-install

    ### Others
    binutils
    gcc
    glibc
    gmp.dev
    icu.dev
    moreutils
    ncurses
    openssl
    pkgconfig
    zlib.dev

    # frontend
    nodejs-12_x
    yarn

    # database
    postgresql
  ];
}
