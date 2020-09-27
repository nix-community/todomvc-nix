# Generated with cabal2nix
{ mkDerivation, aeson, base, http-types, monad-logger, path-pieces
, persistent, persistent-sqlite, persistent-template, resourcet
, stdenv, text, transformers, wai, wai-extra
}:
mkDerivation {
  pname = "todobackend-common";
  version = "0.1.0.0";
  src = ../../backend/haskell/todobackend-common;
  libraryHaskellDepends = [
    aeson base http-types monad-logger path-pieces persistent
    persistent-sqlite persistent-template resourcet text transformers
    wai wai-extra
  ];
  homepage = "http://github.com/jhedev/todobackend-haskell";
  description = "Common stuff (e.g. model) for todobackends";
  license = stdenv.lib.licenses.mit;
}
