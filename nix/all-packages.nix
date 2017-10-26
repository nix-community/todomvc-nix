/**
 * This is an overlay with all the packages and overrides for this repo.
 */
self: super:

with super;

{
  lib = super.lib // {
    path = self.callPackage ./lib/path.nix { };
  };

  mkShell = self.callPackage ./build-support/mk-shell.nix {};

  yarn2nix = self.callPackage ./yarn2nix {};

  inherit (self.yarn2nix) mkYarnPackage linkNodeModulesHook;

  frontend = self.callPackage ./frontend {};

  frontend-docker = self.callPackage ./frontend/docker.nix {};

  haskellPackages = self.callPackage ./backend {};

  backend = self.haskellPackages.todobackend-scotty;

  backend-docker = self.callPackage ./backend/docker.nix {};

  scripts = {
    run = callPackage ./scripts/run.nix { };
  };
}
