final: prev:
rec {
  todomvc = {
    nix = prev.callPackage ./nix/all-packages.nix { };
    backend-rust = prev.naersk.buildPackage {
      src = final.builtins.filterSource (path: type: type != "directory" || final.builtins.baseNameOf path != "target") ./backend/rust;
      remapPathPrefix = true;
    };
  };
}
