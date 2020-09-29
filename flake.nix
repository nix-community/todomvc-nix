{
  description = "todomvc-nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.devshell.url = "github:numtide/devshell";
  inputs.mozilla-overlay = {
    type = "github";
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    flake = false;
  };

  outputs = { self, nixpkgs,  mozilla-overlay, flake-utils, devshell }:
    {
      overlay = import ./overlay.nix;
    }
    //
    (
      flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
            config = {
                allowBroken = true;
            };
            overlays = [
                (import mozilla-overlay)
                devshell.overlay
                self.overlay
            ];
          };
        in
        {
          legacyPackages = pkgs.todomvc.nix;

          defaultPackage = pkgs.todomvc.nix.backend;

          packages = flake-utils.lib.flattenTree pkgs.todomvc.nix;

          devShell = import ./shell.nix { inherit pkgs; };
          # pkgs.mkDevShell.fromTOML ./devshell.toml;

          checks = { };
        }
      )
    );
}
