{ todomvc }:
with todomvc;
#Build a new overlay with our own packages
haskellNixPkgs.haskell-nix.cabalProject {
  projectFileName = "cabal.project";
  src = haskellNixPkgs.haskell-nix.haskellLib.cleanGit {
    name = "todo-haskell";
    src  = ../../backend/haskell;
  };
  compiler-nix-name = "ghc8102";
  index-state = "2020-11-10T00:00:00Z";
#   plan-sha256 = sha256;

  cabalProject =
    builtins.replaceStrings
      [ "with-compiler" ]
      [ "-- with-compiler" ]
      (builtins.readFile ../../backend/haskell/cabal.project);

  modules = [{
    # smaller files
    packages.todo-haskell.dontStrip = false;
    packages.todo-common.dontStrip = false;
  }]; #++
#   haskellNixPkgs.lib.optional haskellNixPkgs.hostPlatform.isMusl {
#     packages.tttool.configureFlags = [ "--ghc-option=-static" ];
#     # terminfo is disabled on musl by haskell.nix, but still the flag
#     # is set in the package plan, so override this
#     packages.haskeline.flags.terminfo = false;
#   };
}
