# TodoMVC-Nix Haskell

After running `nix develop`, there are packages added in the `PATH` for Haskell development, like `cabal-install`, `ghc` and `todo-haskell`.

## Development

Please note that by default, `cabal-install` and `ghc` are from `miso`'s `pkgs` so it can only be used to run Haskell's frontend project. That is why there is `todo-haskell` for Haskell's backend.

### **Backend**

To develop backend, please comment the `ghc` and `cabal-install` package that from `miso` and uncomment the `ghc` and `cabal-install` from `haskellPackages`.

Please refer to [shell.nix](../shell.nix).

### **Frontend**

On development phase, it is better to compile `miso` with `ghc`. Go to frontend haskell folder and run the cabal-install command as follows:

```
$ cd frontend/haskell
$ cabal new-run
```
