/**
 * This is the entry-point for all nix execution in this project.
 */
{ }:
import ./nixpkgs {
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = {
    allowBroken = true;
  };
  overlays = [
    # all the packages are defined there:
    (import ./all-packages.nix)
  ];
}
