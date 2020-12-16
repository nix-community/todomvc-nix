{ todomvc }:
with todomvc.misoPkgs;
let
  haskell-lib = pkgs.haskell.lib;
  noCheck = p: pkgs.haskell.lib.dontCheck p;
  noHaddock = p: pkgs.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
in
pkgs.haskell.packages.ghcjs86.override {
  overrides = final: prev: {
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
    servant = noCheck prev.servant;
    servant-jsaddle = prev.callCabal2nix "servant-jsaddle" todomvc.servant-jsaddle { };
    todo-common = prev.callCabal2nix "todo-common" ./../../haskell/common { };
    todo-miso = prev.callCabal2nix "todo-miso" ./../../haskell/frontend { };
  };
}
