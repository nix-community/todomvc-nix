{ pkgs }:
{
#   myHaskellNixPackages = pkgs.callPackage ./haskell-nix { };
  todoHaskell = pkgs.callPackage ./haskell { };
  rust = pkgs.callPackage ./rust { };
  database = pkgs.callPackage ./database { };
}
