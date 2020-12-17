# TodoMVC-Nix Haskell

After running `nix develop`, there are packages added in the `PATH` for Haskell development, like `cabal-install`, `ghc` and `todo-haskell`.

## Development

For development, there is `cabal.project` file as a starting point so that `cabal` can pick the packages that we need.

To develop backend, just change your directory into `haskell/` directory as follows:

```
$ cd haskell
$ cabal new-run backend
```

To develop frontend, use `frontend` instead of `backend` like:

```
$ cd haskell
$ cabal new-run frontend
```

## Release

For release process, we don't use `cabal.project` anymore. Instead, run the following command to build `backend` or `frontend`:

### For non-flake-nix user:

1. Frontend
```
$ nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.haskellFrontend
```

2. Backend
```
nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.haskellBackend
```


### For flake user:

```
```
