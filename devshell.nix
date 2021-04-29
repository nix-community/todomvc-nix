{ pkgs }:

with pkgs;

# Configure your development environment.
#
# Documentation: https://github.com/numtide/devshell
devshell.mkShell {
  name = "todomvc-nix";
  motd = ''
    Welcome to the todomvc-nix application.

    If you see this message, it means your are inside the Nix shell.

    Command available:
    - pginit: initial PostgreSQL setup
    - pgstart: start psql service
    - pgstop: stop psql service
    - migrate: migrate the database
    - deletdb: remove the database completely.
  '';
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

  env = [
    {
      name = "DATABASE_URL";
      value = "postgresql://todomvc_dbuser:todomvc_dbpass@localhost:5432/todomvc_db";
    }

    {
      name = "PGHOST";
      value = "localhost";
    }

    {
      name = "PGPORT";
      value = "5432";
    }

    {
      name = "PGDATABASE";
      value = "todomvc_db";
    }

    {
      name = "PGUSER";
      value = "todomvc_dbuser";
    }

    {
      name = "PGPASSWORD";
      value = "todomvc_dbpass";
    }

    {
      name = "OPENSSL_DIR";
      value = "${openssl.bin}/bin";
    }

    {
      name = "OPENSSL_LIB_DIR";
      value = "${openssl.out}/lib";
    }

    {
      name = "OPENSSL_INCLUDE_DIR";
      value = "${openssl.out.dev}/include";
    }
  ];

  packages = [
    # Haskell
    # todomvc.reflexDev.ghc.frontend
    todomvc.todoHaskell.haskell.packages.ghc865.cabal-install
    (todomvc.todoHaskell.haskell.packages.ghc865.ghcWithPackages (p: with p; [
      aeson
      aeson-pretty
      http-types
      todo-common
      zlib
      polysemy
      todo-haskell
      unliftio
      wai
      wai-logger
      wai-extra
      servant
      servant-server
      postgresql-simple
      jsaddle
      jsaddle-warp
      transformers
      warp
      websockets
      todo-common
      servant-jsaddle
      miso-jsaddle
      lens
      text
      http-proxy
      http-client
      mtl
    ]))

    # build tools
    ## Rust
    todomvc.nix.rustOverlay
    wasm-bindgen-cli
    binaryen

    # Rust
    ## Backend
    todomvc.nix.rustOverlay

    ### Others
    binutils
    pkgconfig
    openssl
    openssl.dev
    gcc
    glibc
    gmp.dev
    nixpkgs-fmt

    # Javascript related frontend
    # It is also used for Rust's frontend development
    nodejs-12_x
    wasm-pack
    yarn
    yarn2nix

    # database
    postgresql
  ];
}
