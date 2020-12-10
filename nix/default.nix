{ pkgs }:
{
  haskellBackend = pkgs.callPackage ./haskell { };
  haskellMiso = pkgs.callPackage ./haskell-miso { };
  rust = pkgs.callPackage ./rust { };
  rustFrontend = pkgs.callPackage ./rust-frontend { };
  database = pkgs.callPackage ./database { };
}
