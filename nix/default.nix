/**
 * This is the entry-point for all nix execution in this project.
 */
{ nixpkgsSrc ? ./nixpkgs }:
import (import nixpkgsSrc) {
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = {
    allowBroken = true;
  };
  overlays = [
    # all the packages are defined there:
    (import ./all-packages.nix)
  ];
}
