{ todomvc }:
with todomvc;
# todoHaskellObelisk.reflex-platform.project ({ pkgs, ... }: {
#     packages = {
#       frontend = ./frontend;
#       common = ./common;
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

# This should be working fine.
todoHaskellObelisk.project ./. ({ ... }: {
  packages = {
    frontend = ../../frontend/haskell/reflex-dom/frontend;
    common = ../../common/haskell;
  };
  staticFiles = ../../frontend/haskell/reflex-dom/static;
})
