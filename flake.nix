{
  description = "todomvc-nix";
  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.devshell.url = "github:numtide/devshell/main";

  # Rust dependencies
  inputs.naersk.url = "github:nmattia/naersk";
  inputs.naersk.inputs.nixpkgs.follows = "nixpkgs";
  # Only for example, use the .url for simplicity
  inputs.mozilla-overlay.url = "github:mozilla/nixpkgs-mozilla";

  # Haskell dependencies
  # This is haskell's dependencies for both Haskell backend and frontend.
  inputs.polysemy = { url = "github:polysemy-research/polysemy"; flake = false; };
  inputs.http-media = { url = "github:zmthy/http-media/develop"; flake = false; };
  inputs.servant = { url = "github:haskell-servant/servant"; flake = false; };
  inputs.servant-jsaddle = { url = "github:haskell-servant/servant-jsaddle/master"; flake = false; };
  # Miso uses its own nixpkgs, thus we imported it to use its pinned nixpkgs on the `overlay.nix`.
  inputs.miso = { url = "github:dmjio/miso/master"; flake = false; };

  outputs = { self, nixpkgs, naersk, mozilla-overlay, flake-utils, devshell, polysemy, http-media, servant, miso, servant-jsaddle }:
    {
      overlay = import ./overlay.nix { inherit polysemy http-media servant miso servant-jsaddle; };
    }
    //
    (
      flake-utils.lib.eachSystem
        [ "x86_64-linux"
          # "x86_64-darwin"
          "aarch64-linux"
          # "aarch64-darwin"
        ]
        (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
            config = {
              allowBroken = true;
              permittedInsecurePackages = [
                "openssl-1.0.2u"
              ];
            };
            overlays = [
              mozilla-overlay.overlay
              devshell.overlays.default

              naersk.overlay
              self.overlay
            ];
          };
        in
        {
          legacyPackages = pkgs.todomvc;

          packages = flake-utils.lib.flattenTree pkgs.todomvc;

          devShell = import ./devshell.nix { inherit pkgs; };

          checks = { };
        }
      )
    );
}
