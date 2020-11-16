{ haskellNix, haskellHackageNix, haskellStackageNix, beam, polysemy, http-media, servant, stack, rhyolite-obelisk, miso, servant-jsaddle }:
final: prev:
let
  noCheck = p: final.haskell.lib.dontCheck p;
  noHaddock = p: final.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
  obelisk = import rhyolite-obelisk { system = final.system; profiling = true; };
  misoPkgs = import miso { system = final.system; allowBroken = true; };

#   haskell-nix = import final.haskellNix {
#       sourcesOverride = {
#         hackage = haskellHackageNix;
#         stackage = haskellStackageNix;
#       };
#   };
in rec
{
  todomvc = rec {
    inherit polysemy http-media servant servant-jsaddle misoPkgs;
    # TODO: Make haskell.nix works!
    # haskellNixPkgs = haskellNix.legacyPackages.${final.system} // {
    #     overlays = haskellNix.overlays ++ [
    #         (self: super: {
    #             haskell-nix.haskellPackages = self.haskell-nix.haskellPackages // {
    #                 todo-common = prev.callCabal2nix "todo-common" ./common/haskell {};
    #             };
    #             todo-common = self.haskell-nix.cabalProject {
    #               src = self.haskell-nix.haskellLib.cleanGit {
    #                 name = "todo-common";
    #                 src  = ./common/haskell;
    #               };
    #               compiler-nix-name = "ghc8102";
    #               index-state = "2020-11-10T00:00:00Z";
    #               #   plan-sha256 = sha256;

    #               cabalProject =
    #                 builtins.replaceStrings
    #                 [ "with-compiler" ]
    #                 [ "-- with-compiler" ]
    #                 (builtins.readFile ./common/haskell/cabal.project);
    #             };
    #         })
    #     ];
    # };
    todoHaskellObelisk =
      let
        inherit (prev) lib;
        hsLib = prev.haskell.lib;
        composeExtensions = prev.lib.composeExtensions;
        haskellOverrides =  lib.foldr composeExtensions obelisk.haskellOverrides [
          (self: super: with hsLib; {
            beam-core = self.callCabal2nix "beam-core" (beam + "/beam-core") {};
            beam-postgres = noCheck (self.callCabal2nix (beam + "/beam-postgres") {});
            beam-migrate = self.callCabal2nix (beam + "/beam-migrate") {};
            polysemy = self.callCabal2nix "polysemy" polysemy {};
          })
        ];
      in obelisk // {
        inherit haskellOverrides;
        project = base: projectDefinition:
          obelisk.project base ({...}@args:
            let def = projectDefinition args;
            in def // {
              overrides = composeExtensions haskellOverrides (def.overrides or (_: _: {}));
            });
      };

    reflexDev = todoHaskellObelisk.project ./. ({ ... }: {
      packages = {
        ios.bundleIdentifier = null;
        ios.bundleName = null;
        todo-haskell = ./backend/haskell;
        # frontend = ./frontend/reflex-dom;
        todo-common = ./common/haskell;
      };
    });

    # todoHaskellObelisk.reflex-platform.project ({ ... }: {
    #   packages = {
    #     todo-haskell = ./backend/haskell;
    #     # todo-reflex = ./frontend/haskell;
    #     todo-common = ./common/haskell;
    #   };
    #   shells = rec {
    #     ghc = [
    #         "todo-haskell"
    #     ] ++ ghcjs;
    #     ghcjs = [
    #         "todo-common"
    #     ];
    #   };
    # });



    # todoHaskellMisoDev = misoDevPkgs.pkgs.haskell.packages.ghc865;
    misoDev = (misoPkgs.pkgs.haskell.packages.ghc865.override {
      all-cabal-hashes = misoPkgs.pkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/8c7bdd9ad4bc3671b4214e32766873d443af2622.tar.gz";
        sha256 = "0q9qdpvn3c64rwnafcqkzzyi4z72mvvwmvn06d89fnzfpqjxvwx2";
      };
      }).extend (self: super: {
        clay = self.callHackage "clay" "0.13.3" {};
        websockets = self.callHackage "websockets" "0.12.6.0" {};
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
    # this `todoObelisk` is not needed once `todoHaskellObelisk` in ./nix/haskell-obelisk works.
    # todoObelisk = final.callPackage ./frontend/haskell/reflex-dom { };
    # inherit polysemy http-media servant stack;
    todoHaskellPackages = prev.haskell.packages.ghc8102.extend (self: super: {
      http-media = self.callCabal2nix "http-media" http-media {};
      pantry = noCheck (self.callHackage "pantry" "0.5.1.3" {});
      polysemy = self.callCabal2nix "polysemy" polysemy {};
      stack = self.callCabal2nix "stack" stack {};
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
