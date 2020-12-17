{ todomvc }:
# We declare `with todomvc.misoPkgs` in order to avoid writing
# `todomvc.misoPkgs.pkgs` in every line. In this case, `pkgs` used
# in this file is `pkgs` attribute from `misoPkgs`.
with todomvc.misoPkgs;
let
  haskell-lib = pkgs.haskell.lib;
  noCheck = p: pkgs.haskell.lib.dontCheck p;
  noHaddock = p: pkgs.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
in
((pkgs.haskell.packages.ghcjs86.override {
    all-cabal-hashes = pkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/90e9a5c0282099dd8aa5a369b2b474d0dc354ab8.tar.gz";
        sha256 = "sha256-2bEC/2b+Fa+yCg72upOHKQtEzCbf6lYjpTN0nT23nZw=";
      };
}).extend (final: prev: {
    doctest = null;
    QuickCheck = noCheck prev.QuickCheck;
    tasty-quickcheck = noCheck prev.tasty-quickcheck;
    scientific = noCheck prev.scientific;
    base-compat-batteries = noCheck prev.base-compat-batteries;

    # A bunch of things use doctest in their tests, but doctest doesn't appear
    # to be able to be compiled, so we just disable tests for all these.
    comonad = noCheck prev.comonad;
    lens = noCheck prev.lens;
    semigroupoids = noCheck prev.semigroupoids;
    http-types = noCheck prev.http-types;
    servant = prev.callHackage "servant" "0.16" { };
    servant-client-core = prev.callHackage "servant-client-core" "0.16" { };
    servant-jsaddle = final.callCabal2nix "servant-jsaddle" todomvc.servant-jsaddle { };
    todo-common = prev.callCabal2nix "todo-common" ./../../haskell/common { };
    todo-miso = prev.callCabal2nix "todo-miso" ./../../haskell/frontend { };
})).todo-miso

