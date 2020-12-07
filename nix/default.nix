{ pkgs }:
{
  haskellBackend = pkgs.callPackage ./haskell { };
  haskellMiso = pkgs.callPackage ./haskell-miso { };
  rust = pkgs.callPackage ./rust { };
  database = pkgs.callPackage ./database { };
}
