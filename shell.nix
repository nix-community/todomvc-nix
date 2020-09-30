{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkDevShell {
  name = "todomvc-nix";
  motd = "otherthing";
  commands = [
    {
      name = "pgstart";
      help = "start psql service";
      command = "${todomvc.nix.pkgs.database.pgutil.start_pg} || echo '''PG start failed''' ";
    }
    {
      name = "pgstop";
      help = "stop psql service";
      command = "${todomvc.nix.pkgs.database.pgutil.stop_pg} || echo '''PG start failed''' ";
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
    CARGO="${todomvc.nix.pkgs.rust}/cargo";
    PGHOST="localhost";
    PGPORT="5432";
    PGDATABASE="todomvc_db";
    PGUSER="todomvc_dbuser";
    PGPASSWORD="todomvc_dbpass";
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
    binutils
	gcc

    # backend
    todomvc.nix.backend

    # frontend

    # database
    #sqitchPg
    postgresql
    # todomvc.nix.pkgs.postgresql.
    moreutils
  ];

}
