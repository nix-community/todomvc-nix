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
                # all the packages are defined there:
                # (import ./nix/all-packages.nix)
                (import mozilla-overlay)
                devshell.overlay
                self.overlay
            ];
          };
        #   import ./nix { nixpkgsSrc = nixpkgs; };
        in
        {
          legacyPackages = pkgs.todomvc.nix;

          defaultPackage = pkgs.todomvc.nix.backend;

          packages = flake-utils.lib.flattenTree pkgs.todomvc.nix;

          devShell =
            pkgs.mkDevShell.fromTOML ./devshell.toml;
            # //
            # {
            #     shellHook = ''
            #       export GO111MODULE=on
            #       export Andika=Andika
            #       unset GOPATH GOROOT
            #     '';
            # };
        #   import ./shell.nix { inherit pkgs; };

          # Additional checks on top of the packages
          checks = { };
        }
      )
    );
}
