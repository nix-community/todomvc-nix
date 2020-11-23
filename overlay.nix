{ polysemy, http-media, servant, miso, servant-jsaddle }:
final: prev:
let
  noCheck = p: final.haskell.lib.dontCheck p;
  noHaddock = p: final.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
  misoPkgs = import miso { system = final.system; allowBroken = true; };

in rec
{
  todomvc = rec {
    inherit polysemy http-media servant servant-jsaddle misoPkgs;
    misoDev = (misoPkgs.pkgs.haskell.packages.ghc865.override {
      all-cabal-hashes = misoPkgs.pkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/8c7bdd9ad4bc3671b4214e32766873d443af2622.tar.gz";
        sha256 = "0q9qdpvn3c64rwnafcqkzzyi4z72mvvwmvn06d89fnzfpqjxvwx2";
      };
      }).extend (self: super: {
        clay = self.callHackage "clay" "0.13.3" {};
        websockets = self.callHackage "websockets" "0.12.6.0" {};
        http-client = self.callHackage "http-client" "0.6.4.1" {};
        http-proxy = fast super.http-proxy;
        servant-client-core = self.callHackage "servant-client-core" "0.16" {};
        servant = self.callHackage "servant" "0.16" {};
        servant-server = self.callHackage "servant-server" "0.16" {};
        servant-lucid = self.callHackage "servant-lucid" "0.9" {};
        servant-jsaddle = noCheck (self.callCabal2nix "servant-jsaddle" servant-jsaddle {});
        jsaddle-warp = fast super.jsaddle-warp;
        todo-common = self.callCabal2nix "todo-common" ./common/haskell { };
        todo-miso = self.callCabal2nix "todo-miso" ./../../frontend/haskell/miso { miso = misoPkgs.miso-jsaddle; };
      }
    );
    todoHaskellMisoDev = misoPkgs.pkgs // {
      haskell = misoPkgs.pkgs.haskell // {
          packages = misoPkgs.pkgs.haskell.packages // {
              ghc865 = misoDev;
          };
      };
    };

    todoHaskellPackages = prev.haskell.packages.ghc8102.extend (self: super: {
      http-media = self.callCabal2nix "http-media" http-media {};
      pantry = noCheck (self.callHackage "pantry" "0.5.1.3" {});
      polysemy = self.callCabal2nix "polysemy" polysemy {};
      servant = self.callCabal2nix "servant" (servant + "/servant") {};
      servant-server = self.callCabal2nix "servant-server" (servant + "/servant-server") {};
      time-compat = fast super.time-compat;
      todo-common = self.callCabal2nix "todo-common" ./common/haskell {};
      todo-haskell = self.callCabal2nix "todo-haskell" ./backend/haskell {};
    });
    nix = prev.callPackage ./nix { };
    backend-rust = prev.naersk.buildPackage {
      src = final.builtins.filterSource (path: type: type != "directory" || final.builtins.baseNameOf path != "target") ./backend/rust;
      remapPathPrefix = true;
    };

  };

}
