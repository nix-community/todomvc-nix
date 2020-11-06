{ haskell }:
let
  noCheck = p: haskell.lib.dontCheck p;
  noHaddock = p: haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
in
#Build a new overlay with our own packages
haskell.packages.ghc884.extend (self: super: {
  polysemy-plugin = self.callHackage "polysemy-plugin" "0.2.5.1" {};
  todomvc-haskell = self.callCabal2nix "todo-haskell" ../../backend/haskell {};
})
