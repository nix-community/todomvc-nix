{ todomvc }:
with todomvc.misoPkgs;
let
  haskell-lib = pkgs.haskell.lib;
  noCheck = p: pkgs.haskell.lib.dontCheck p;
  noHaddock = p: pkgs.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
in
# `pkgs` in this `default.nix` is from `todomvc.misoPkgs`
(pkgs.haskell.packages.ghcjs86.override {
  all-cabal-hashes = misoPkgs.pkgs.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/8c7bdd9ad4bc3671b4214e32766873d443af2622.tar.gz";
    sha256 = "0q9qdpvn3c64rwnafcqkzzyi4z72mvvwmvn06d89fnzfpqjxvwx2";
  };
}).extend (self: super: {
  doctest = null;
  QuickCheck = noCheck super.QuickCheck;
  tasty-quickcheck = noCheck super.tasty-quickcheck;
  scientific = noCheck super.scientific;
  base-compat-batteries = noCheck super.base-compat-batteries;
  # A bunch of things use doctest in their tests, but doctest doesn't appear
  # to be able to be compiled, so we just disable tests for all these.
  comonad = noCheck super.comonad;
  lens = noCheck super.lens;
  semigroupoids = noCheck super.semigroupoids;
  http-types = noCheck super.http-types;
  servant = noCheck super.servant;
  servant-jsaddle = super.callCabal2nix "servant-jsaddle" todomvc.servant-jsaddle { };
  todo-common = super.callCabal2nix "todo-common" ./../../haskell/common { };
  todo-miso = super.callCabal2nix "todo-miso" ./../../haskell/frontend { };
})
