/**
 * This is the entry-point for all nix execution in this project.
 *
 * TODO: pin <nixpkgs>
 */
import <nixpkgs> {
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = { };
  overlays = [
    # all the packages are defined there:
    (import ./all-packages.nix)
  ];
}
