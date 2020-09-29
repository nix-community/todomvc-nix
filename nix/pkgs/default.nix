{ pkgs }:
{
  rust = pkgs.callPackage ./rust { };
  postgresql = pkgs.callPackage ./postgresql {};
}
