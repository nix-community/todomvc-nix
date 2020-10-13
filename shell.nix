{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkDevShell {
  name = "todomvc-nix";
  motd = "otherthing";
  commands = [
    {
      name = "pginit";
      help = "init psql service";
      command = "${todomvc.nix.pkgs.database.pgutil.init_pg} || echo '''PG init failed''' ";
    }
    {
      name = "pgstart";
      help = "start psql service";
      command = "${todomvc.nix.pkgs.database.pgutil.start_pg} || echo '''PG start failed''' ";
    }
    {
      name = "pgstop";
      help = "stop psql service";
      command = "${todomvc.nix.pkgs.database.pgutil.stop_pg} || echo '''PG stop failed''' ";
    }
    {
      name = "migrate";
      help = "migrate database using sqitch";
      command = "${todomvc.nix.pkgs.database.migrate}/bin/sqitch deploy || echo '''Migrate database failed''' ";
    }
  ];

  bash = {
    extra = ''
      unset GOPATH GOROOT
    '';
    interactive = ''
    '';
  };

  env = {
    RUST_SRC_PATH="${todomvc.nix.pkgs.rust}";
    RUSTFLAGS="-C linker=${lld_9}/bin/lld";
    CARGO="${todomvc.nix.pkgs.rust}/cargo";
    DATABASE_URL="postgresql://todomvc_dbuser:todomvc_dbpass@localhost:5432/todomvc_db";
    PGHOST="localhost";
    PGPORT="5432";
    PGDATABASE="todomvc_db";
    PGUSER="todomvc_dbuser";s
    PGPASSWORD="todomvc_dbpass";
    LD_LIBRARY_PATH="${stdenv.cc.cc.lib}/lib64:\$LD_LIBRARY_PATH";
  };

  packages = [
    # build tools
    ### Rust
	todomvc.nix.pkgs.rust

    ### Haskell
    haskellPackages.ghc

    ### Go
    go
    gopls
    gopkgs
    gocode
    go-outline

    ### Others
    glibc

    # backend
    todomvc.nix.backend

    # frontend
    yarn
    yarn2nix
    wasm-pack

    # database
    #sqitchPg
    postgresql
    # todomvc.nix.pkgs.postgresql.
    moreutils
  ];

}
