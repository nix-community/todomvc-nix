{ pkgs }:
{
  haskellNixBackend = pkgs.callPackage ./haskell-nix { };
  haskellBackend = pkgs.callPackage ./haskell { };
  haskellObelisk = pkgs.callPackage ./haskell-obelisk { };
  haskellMiso = pkgs.callPackage ./haskell-miso { };
  rust = pkgs.callPackage ./rust { };
  database = pkgs.callPackage ./database { };
}
