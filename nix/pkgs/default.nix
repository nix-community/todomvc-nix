{ pkgs }:
{
  rust = pkgs.callPackage ./rust { };
  database = pkgs.callPackage ./database {};
}
