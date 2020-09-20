{
  description = "todomvc-nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";


  outputs = { self, nixpkgs, flake-utils }:
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
                (import ./nix/all-packages.nix)
            ];
          };
        #   import ./nix { nixpkgsSrc = nixpkgs; };
        in
        {
          legacyPackages = pkgs.backend;

          defaultPackage = pkgs.backend;

          packages = flake-utils.lib.flattenTree pkgs.backend;

          devShell = import ./shell.nix { inherit pkgs; };

          # Additional checks on top of the packages
          checks = { };
        }
      )
    );
}
