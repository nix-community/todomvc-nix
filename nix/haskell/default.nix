{ haskell, pkgs, todomvc }:
let
  noCheck = p: haskell.lib.dontCheck p;
  noHaddock = p: haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
in
#Build a new overlay with our own packages
with todomvc;

todoHaskellPackages.developPackage {
    root = ../../backend/haskell;
    name = "todo-haskell";
    source-overrides = {
        polysemy = todomvc.polysemy;
        http-media = todomvc.http-media;
        servant = todomvc.servant + "/servant";
        servant-server = todomvc.servant + "/servant-server";
        todo-common = ../../common/haskell;
    };
    overrides = self: super: {
        time-compat = fast super.time-compat;
        todo-common = fast super.todo-common;
    };
}
