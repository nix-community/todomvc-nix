{
  description = "todomvc-nix";
  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.devshell.url = "github:numtide/devshell/master";
  inputs.naersk.url = "github:nmattia/naersk";
  # Only for example, use the .url for simplicity
  inputs.mozilla-overlay = {
    type = "github";
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    flake = false;
  };

  # Haskell dependencies
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.polysemy = { url = "github:polysemy-research/polysemy"; flake = false; };
  inputs.http-media = { url = "github:zmthy/http-media/develop"; flake = false; };
  inputs.servant = { url = "github:haskell-servant/servant"; flake = false; };
  inputs.miso = { url = "github:dmjio/miso/master"; flake = false; };
  inputs.servant-jsaddle = { url = "github:haskell-servant/servant-jsaddle/master"; flake = false; };

  outputs = { self, nixpkgs, naersk, mozilla-overlay, flake-utils, devshell, polysemy, http-media, servant, miso, servant-jsaddle }:
    {
      overlay = import ./overlay.nix { inherit  polysemy http-media servant miso servant-jsaddle; };
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
                permittedInsecurePackages = [
                  "openssl-1.0.2u"
                ];
            };
            overlays = [
                (import mozilla-overlay)
                devshell.overlay

                naersk.overlay
                self.overlay
             ];
          };
        in
        {
          legacyPackages = pkgs.todomvc;

          defaultPackage = pkgs.todomvc.nix.haskellBackend;

          packages = flake-utils.lib.flattenTree pkgs.todomvc;

          devShell = import ./devshell.nix { inherit pkgs; };

          checks = { };
        }
      )
    );
}
