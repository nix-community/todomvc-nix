{ pkgs }:
{
  haskellBackend = pkgs.callPackage ./haskell { };
  haskellMiso = pkgs.callPackage ./haskell-miso { };
  rustOverlay = pkgs.callPackage ./rust-overlay { };
  rustFrontend = pkgs.callPackage ./rust-frontend { };
  database = pkgs.callPackage ./database { };
}
