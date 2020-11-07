{ haskell-nix }:
#Build a new overlay with our own packages
haskell-nix.cabalProject {
  projectFileName = "cabal.project";
  src = haskell-nix.haskellLib.cleanGit {
    name = "todo-haskell";
    src  = ../../backend/haskell;
  };
  compiler-nix-name = "ghc8102";
}
