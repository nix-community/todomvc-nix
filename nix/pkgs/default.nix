{ pkgs }:
with pkgs;
{
  rust = callPackage ./rust { };
}
