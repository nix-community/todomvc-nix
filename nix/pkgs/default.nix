{ pkgs }:
{
  myHaskellNixPackages = pkgs.callPackage ./haskell-nix { };
  myHaskellPackages = pkgs.callPackage ./haskell { };
  rust = pkgs.callPackage ./rust { };
  database = pkgs.callPackage ./database { };
}
