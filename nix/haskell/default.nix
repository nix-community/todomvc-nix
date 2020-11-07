{ haskell, pkgs, todomvc }:
let
  noCheck = p: haskell.lib.dontCheck p;
  noHaddock = p: haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
in
#Build a new overlay with our own packages
with todomvc;
todoHaskellPackages.shellFor {
  packages = p: [ p.todo-common p.todo-haskell p.aeson ];
  withHoogle = true;
  buildInputs = with pkgs; [
    todoHaskellPackages.cabal-install
    todoHaskellPackages.ghcid
    todoHaskellPackages.haskell-language-server
    todoHaskellPackages.hlint
    todoHaskellPackages.stack
    todoHaskellPackages.ormolu
    zlib.dev
  ];
}

# .developPackage {
#     root = ../../backend/haskell;
#     name = "todo-haskell";
#     source-overrides = {
#         # polysemy = todomvc.polysemy;
#         # http-media = todomvc.http-media;
#         # stack = todomvc.stack;
#         # servant = todomvc.servant + "/servant";
#         # servant-server = todomvc.servant + "/servant-server";
#         todo-common = ../../common/haskell;
#     };
#     overrides = self: super: {
#         # http-media = ghc.http-media;
#         time-compat = fast super.time-compat;
#         todo-common = fast super.todo-common;
#     };
#     modifier = drv:
#       haskell.lib.addBuildTools drv (with ghc; [
#         cabal-install
#         ghc
#         ghcid
#         haskell-language-server
#         hlint
#         todomvc.stack
#         ormolu
#       ]);
# }
