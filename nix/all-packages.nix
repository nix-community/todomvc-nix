/**
 * This is an overlay with all the packages and overrides for this repo.
 */
self: super:

with super;

{
  yarn2nix = self.callPackage ./yarn2nix {};

  inherit (self.yarn2nix) mkYarnPackage;

  frontend = self.callPackage ./frontend {};

  backend = self.callPackage ./backend {};

}
