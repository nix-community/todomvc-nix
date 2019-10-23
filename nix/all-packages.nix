/**
 * This is an overlay with all the packages and overrides for this repo.
 */
self: super:

with super;

{
  lib = lib // {
    path = self.callPackage ./lib/path.nix { };
  };

  fetchJSON = self.callPackage ./build-support/fetch-json.nix {};

  yarn2nix-src = self.fetchJSON ./yarn2nix-src.json;

  yarn2nix = self.callPackage self.yarn2nix-src {};

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
