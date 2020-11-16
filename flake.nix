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
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  inputs.haskellHackageNix = { url = "github:input-output-hk/hackage.nix/master"; flake = false; };
  inputs.haskellStackageNix = { url = "github:input-output-hk/stackage.nix/master"; flake = false; };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.polysemy = { url = "github:polysemy-research/polysemy"; flake = false; };
  inputs.http-media = { url = "github:zmthy/http-media/develop"; flake = false; };
  inputs.servant = { url = "github:haskell-servant/servant"; flake = false; };
  inputs.stack = { url = "github:commercialhaskell/stack"; flake = false; };
  inputs.rhyolite-obelisk = { url = "github:obsidiansystems/rhyolite/develop"; flake = false; };
  inputs.miso = { url = "github:dmjio/miso/master"; flake = false; };
  inputs.servant-jsaddle = { url = "github:haskell-servant/servant-jsaddle/master"; flake = false; };
  inputs.beam = { url = "github:haskell-beam/beam/master"; flake = false; };

  outputs = { self, nixpkgs, naersk, mozilla-overlay, haskellNix, haskellHackageNix, haskellStackageNix, flake-utils, devshell, beam, polysemy, http-media, servant, stack, rhyolite-obelisk, miso, servant-jsaddle }:
    {
      overlay = import ./overlay.nix { inherit haskellNix haskellHackageNix haskellStackageNix beam polysemy http-media servant stack rhyolite-obelisk miso servant-jsaddle; };
    }
    //
    (
      flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
        let

            # let
            #     hnixoverlay = (haskellNix.overlays {
            #       sourcesOverride = {
            #         hackage = haskellHackageNix;
            #         stackage = haskellStackageNix;
            #       };
            #     }).combined-eval-on-build;
            #     nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
            #     nixpkgsArgs = {
            #       config = haskellNix.config;
            #       overlays = [ hnixoverlay ];
            #     };
                # legacyPackages =
                # let
                #   genAttrs = lst: f:
                #     builtins.listToAttrs (map (name: {
                #       inherit name;
                #       value = f name;
                #     }) lst);
                # in
                # genAttrs [ "x86_64-linux" "x86_64-darwin" ] (system:
                #   import self.sources.nixpkgs
                #   (self.nixpkgsArgs // { localSystem = { inherit system; }; })
                # );
            #     hnixpkgs = import nixpkgsSrc (nixpkgsArgs // { localSystem = { inherit system; }; });
            # in hnixpkgs;

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

          defaultPackage = pkgs.todomvc.nix.haskellMiso.dev.todo-miso;

          packages = flake-utils.lib.flattenTree pkgs.todomvc;

          devShell = import ./shell.nix { inherit pkgs; };

          checks = { };
        }
      )
    );
}
