{ haskell-nix }:
#Build a new overlay with our own packages
haskell-nix.project {
  projectFileName = "cabal.project";
  src = haskell-nix.haskellLib.cleanGit {
    name = "todomvc-haskellNix";
    src  = ../../backend/haskell;
  };
  compiler-nix-name = "ghc884";
}
