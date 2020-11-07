{ polysemy, http-media, servant, stack }:
final: prev:
let
  noCheck = p: final.haskell.lib.dontCheck p;
  noHaddock = p: final.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
in
rec {
  todomvc = {
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
