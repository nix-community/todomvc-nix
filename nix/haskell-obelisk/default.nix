{ todomvc }:
with todomvc;
# todoHaskellObelisk.reflex-platform.project ({ pkgs, ... }: {
#     packages = {
#       frontend = ./../../frontend/haskell/reflex-dom/frontend;
#       common = ./../../common/haskell;
#     };
#     shells = rec {
#       ghc = [
#       ] ++ ghcjs;
#       ghcjs = [
#         "frontend"
#         "common"
#       ];
#     };
#     tools = ghc: [ pkgs.postgresql ];
# })
todoHaskellObelisk.project ./. ({ ... }: {
  packages = {
    ios.bundleIdentifier = null;
    ios.bundleName = null;
    todo-reflex = ./.;
  };
})
