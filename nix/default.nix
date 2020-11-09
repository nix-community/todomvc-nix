{ pkgs }:
{
#   myHaskellNixPackages = pkgs.callPackage ./haskell-nix { };
  todoHaskell = pkgs.callPackage ./haskell { };
  haskellObelisk = pkgs.callPackage ./haskell-obelisk { };
  rust = pkgs.callPackage ./rust { };
  database = pkgs.callPackage ./database { };
}
