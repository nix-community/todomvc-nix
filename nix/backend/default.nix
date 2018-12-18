{ haskell }:
# Build a new overlay with our own packages
haskell.packages.ghc844.extend (self: super: {
  todobackend-common = self.callPackage ./todobackend-common.nix {};
  todobackend-scotty = self.callPackage ./todobackend-scotty.nix {};
})
