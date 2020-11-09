{ beam, polysemy, http-media, servant, stack, rhyolite-obelisk }:
final: prev:
let
  noCheck = p: final.haskell.lib.dontCheck p;
  noHaddock = p: final.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
  obelisk = import rhyolite-obelisk { system = final.system; profiling = true; };
in
{
  todomvc = rec {
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
    # this `todoObelisk` is not needed once `todoHaskellObelisk` in ./nix/haskell-obelisk works.
    # todoObelisk = prev.callPackage ./frontend/haskell/reflex-dom { };
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
