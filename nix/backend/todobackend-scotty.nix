{ mkDerivation, base, http-types, path-pieces, persistent-sqlite
, scotty, stdenv, todobackend-common, transformers
}:
mkDerivation {
  pname = "todobackend-scotty";
  version = "0.1.0.0";
  src = ../../backend/todobackend-scotty;
  isLibrary = false;
  isExecutable = true;
  enableSharedExecutables = false;
  executableHaskellDepends = [
    base http-types path-pieces persistent-sqlite scotty
    todobackend-common transformers
  ];
  description = "Todobackend implementation using Scotty";
  license = stdenv.lib.licenses.mit;
}
