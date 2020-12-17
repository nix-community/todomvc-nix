{ polysemy, http-media, servant, miso, servant-jsaddle }:
final: prev:
let
  noCheck = p: final.haskell.lib.dontCheck p;
  noHaddock = p: final.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
  misoPkgs = import miso { system = final.system; allowBroken = true; };
in
{
  todomvc = rec {
    # This will make all package available using `todomvc.<package-name>`
    inherit polysemy http-media servant servant-jsaddle misoPkgs;
    haskellPkg = (misoPkgs.pkgs.haskell.packages.ghc865.override {
      all-cabal-hashes = misoPkgs.pkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/90e9a5c0282099dd8aa5a369b2b474d0dc354ab8.tar.gz";
        sha256 = "sha256-2bEC/2b+Fa+yCg72upOHKQtEzCbf6lYjpTN0nT23nZw=";
      };
    }).extend (self: super: {
      aeson = noCheck (self.callHackage "aeson" "1.4.4.0" { });
      clay = self.callHackage "clay" "0.13.3" { };
      websockets = self.callHackage "websockets" "0.12.6.0" { };
      http-client = self.callHackage "http-client" "0.6.4.1" { };
      http-proxy = fast super.http-proxy;
      servant-client-core = self.callHackage "servant-client-core" "0.16" { };
      servant = self.callHackage "servant" "0.16" { };
      servant-server = self.callHackage "servant-server" "0.16" { };
      servant-lucid = self.callHackage "servant-lucid" "0.9" { };
      servant-jsaddle = noCheck (self.callCabal2nix "servant-jsaddle" servant-jsaddle { });
      jsaddle-warp = fast super.jsaddle-warp;
      todo-common = self.callCabal2nix "todo-common" ./haskell/common { };
      todo-miso = self.callCabal2nix "todo-miso" ./haskell/frontend { miso = misoPkgs.miso-jsaddle; };
      # Backend specific dependencies.
      #
      # We build pantry to first-class-families because these packages are
      # polysemy dependencies which is not in GHC 8.6.5 package set.
      pantry = noCheck (self.callHackage "pantry" "0.5.1.3" { });
      type-errors = self.callHackage "type-errors" "0.2.0.0" { };
      type-errors-pretty = self.callHackage "type-errors-pretty" "0.0.1.1" { };
      first-class-families = self.callHackage "first-class-families" "0.5.0.0" { };
      th-abstraction = self.callHackage "th-abstraction" "0.3.1.0" { };
      th-lift = self.callHackage "th-lift" "0.8.0.1" { };
      polysemy = fast (self.callHackage "polysemy" "1.4.0.0" { });
      time-compat = fast (self.callHackage "time-compat" "1.9.2.2" { });
      todo-haskell = self.callCabal2nix "todo-haskell" ./haskell/backend { };
    });
    todoHaskell = misoPkgs.pkgs // {
      haskell = misoPkgs.pkgs.haskell // {
        packages = misoPkgs.pkgs.haskell.packages // {
          ghc865 = haskellPkg;
        };
      };
    };
    nix = prev.callPackage ./nix { };
  };
}
