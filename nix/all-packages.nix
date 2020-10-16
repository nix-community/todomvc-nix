/**
 * This is an overlay with all the packages and overrides for this repo.
 */
{ callPackage }:
rec {
#   lib = lib // {
#     path = self.callPackage ./lib/path.nix { };
#   };

#   fetchJSON = self.callPackage ./build-support/fetch-json.nix {};

#   yarn2nix-src = self.fetchJSON ./yarn2nix-src.json;

#   yarn2nix = self.callPackage self.yarn2nix-src {};

#   inherit (self.yarn2nix) mkYarnPackage linkNodeModulesHook;

#   frontend = self.callPackage ./frontend {};

#   frontend-docker = self.callPackage ./frontend/docker.nix {};
  recurseForDerivations = true;

#   haskellPackages = callPackage ./backend/haskell.nix {};

#   backend = haskellPackages.todobackend-scotty;

#   backend-docker = callPackage ./backend/docker.nix { inherit backend; };

  pkgs = callPackage ./pkgs {};
#   scripts = {
#     run = callPackage ./scripts/run.nix { };
#   };
}
