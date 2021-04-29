# TodoMVC-Nix Nix Language

This section explains all the details about Nix and how it is being used in this project. To understand about the project, you have to at least familiar with Nix programming language. If you are new to Nix, then we suggest you to follow [this tutorial](https://nixos.org/learn.html).

We make structure of the documentation based on `.nix` file so it can be easy to understand.

## Structure
* [default.nix](#-defaultnix-)
* [shell.nix](#-shellnix-)
* [flake.nix](#-flakenix-)
* [overlay.nix](#-overlaynix-)
* [nix/](#-nix-)
  * [default.nix](#-nix-)
  + [database/](#-database-)
    + [default.nix](#-database-)
    + [migrate.nix](#-migratenix-)
    + [pgutil.nix](#-pgutilnix-)
  + [haskell/](#-haskell-)
    + [default.nix](#-haskell-)
  + [haskell-miso/](#-haskell-miso-)
    + [default.nix](#-haskell-miso-)
  + [rust-overlay/](#-rust-overlay-)
    + [default.nix](#-rust-overlay-)
  + [rust-frontend/](#-rust-frontend-)
    + [default.nix](#-rust-frontend-)
* [devshell.nix](#-devshellnix-)

----------------------------
## **| default.nix |**
[[Back to the Table of Contents] ↑](#structure)

The key to understand Nix is that `default.nix` is the gateway for every Nix project that you want to build. So you basically need to have one `.nix` file, but `default.nix` will saves you a lot of time.

 There are at least two advantages that you will get if you have `default.nix`. Of course you can make another `.nix` file let say `mynondefaultstartingpoint.nix`, but here is why we suggest you to have `default.nix` in your root project or folder:

 1. You do not have to write that `mynon....nix` file everytime you build your project using `nix build`. So you can just run `nix build`, instead of :

     ```
     $ nix build mynondefaultstartingpoint.nix
     ```
 2. You do not have to write that `mynon...nix` everytime you import your project into another `.nix` file. For example (see [here](../nix/default.nix)):

    ```
    { pkgs }:
    {
      haskellBackend = pkgs.callPackage ./haskell { };
      haskellMiso = pkgs.callPackage ./haskell-miso { };
      rust = pkgs.callPackage ./rust { };
      rustFrontend = pkgs.callPackage ./rust-frontend { };
      database = pkgs.callPackage ./database { };
    }
    ```
    In this case, if you do not have `default.nix`, you need to write `rust = pkgs.callPackage ./rust/mynondefaultstartingpoint.nix { };` which is cumbersome.

Meanwhile, *you do not need `default.nix` in your root folder if you want to use `flakes` feature*. However, for the non-flakes-nix distribution we make `default.nix` so user can just run:

```
$ nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.rustBackend
```

If we take a look at the project's default.nix here ([see more](../default.nix)):

```
{ system ? builtins.currentSystem }:
let
  flake-compat = builtins.fetchurl { ... };
in
import flake-compat {
  src = ./.;
  inherit system;
}
```

We use `flake-compat` to make `flake.nix` available for `nix-build` command. One thing that need to be remembered is that you should add `defaultNix.legacyPackages.<system>` on every `nix-build`. The system can be `x86_64-linux` as mentioned above.

---------------------------
## **| shell.nix |**
[[Back to the Table of Contents] ↑](#structure)

The `shell.nix` file has the same purpose with `default.nix` but for `nix-shell` command. If we look into the file, it just import `default.nix` file in the same directory, which as we mentioned earlier about the advantage of having `default.nix`, we can just use `import ./. {};` like the following `shell.nix` file:

```
{ system ? builtins.currentSystem }:
(import ./. { inherit system; }).shellNix
```

Basically, `flake-compat` has two outputs, `defaultNix` and `shellNix`.

--------------------
## **| flake.nix |**
[[Back to the Table of Contents] ↑](#structure)

For nix with flake feature, `flake.nix` is a must. If you want to read more about this feature, we suggest you to read Tweag's blog that explained about the feature in [Part 1](https://www.tweag.io/blog/2020-05-25-flakes/), [Part 2](https://www.tweag.io/blog/2020-06-25-eval-cache), and [Part 3](https://www.tweag.io/blog/2020-07-31-nixos-flakes).

After you read Tweag's articles above, you should have clear understanding about the feature.

For this project, we would like to make it very simple so you can easily grasp the content of `flake.nix`. First, you need to remember that  `flake.nix` has two attributes, `inputs` and `outputs`.

---------------------------
### **| Flake's inputs attribute |**

As you can see below, we have `inputs` ([read more](../flake.nix)):
```
{
  description = "todomvc-nix";
  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils = {
      url = "github:numtide/flake-utils";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
  };
  ...
}
```

which is like every derivation that you want to have in your flakes' project. The syntax is `inputs.<your-chosen-name>.<attribute>`. You can choose whatever name in `<your-chosen-name>` and for the `<attribute>` you have at least:
   1. `url`: for targeting the third-party source code,
   2. `inputs`: if you want to refer to other derivation in `flake.nix`, and
   3. `flake`: Whether you third-party source code have `flake.nix` or not. You can set it to `false` if `flake.nix` is not available.
--------------------------
### **| Flake's outputs attribute |**

`Outputs` attribute contains your project's entry point. It can be used by other project that use flakes feature, or it will be used when you run `nix build`, `nix shell`, or `nix develop` commands.

Here is the snippet of the `outputs` attribute:
```
outputs = { self, nixpkgs, naersk, ... }: --- (1)
    {
      overlay = import ./overlay.nix { inherit  polysemy http-media servant miso servant-jsaddle; }; --- (2)
    }
    //
    (
      flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: --- (3)
        let
          pkgs = import nixpkgs { --- (4)
            inherit system;
            config = { ... }; --- (5)
            overlays = [ ]; --- (6)
          };
        in
        {
          legacyPackages = pkgs.todomvc; --- (7)
          packages = flake-utils.lib.flattenTree pkgs.todomvc; --- (8)
          devShell = import ./devshell.nix { inherit pkgs; }; --- (9)
          checks = { }; --- (10)
        }
      )
    );
```

We have simplify the code a bit and just put reference number for better explanation ([read more](../flake.nix)):

**(1)** This is the starting point of `outputs`. We put every `inputs` that we want to use here so it is available inside `outputs` derivation.

**(2)** The `overlay` attribute makes every package that we listed in `overlay.nix` available for our `nixpkgs` environment by putting it in `overlays` attribute in line 6.

**(3)** We use `flake-utils` to build a collection of pure Nix functions that don't depend on nixpkgs. The better way to understand it is if you refer to Tweag's blog Part 1, you have to use `defaultPackage.x86_64-linux` in your `outputs` result. With `flake-utils` it has function that make your life more simple by just adding the system in the `flake-utils`'s `eachSystem` argument.

**(4)** We import `nixpkgs` environment. The `nixpkgs` that we use is based on line 1 above.

**(5)** Similar to common `nixpkgs` derivation, you can put system's `config` here (e.g. `allowBroken = true;`).

**(6)** If you refer to this project's overlay, you can see that we put every third-party's overlay in here, such as `devshell.overlay`, `naersk.overlay`, and `mozilla-overlay`. Note that we use `import` on `mozilla-overlay` because flakes feature is not available yet.

**(7)** `legacyPcakages` is being used to hold the current implementation of nixpkgs. It can hold any value (e.g. derivation, number, etc.) and when we run `nix-build`, then `nix-build` will build all attributes in the attributes set. It will also scan attribute for spceial property and build the whole tree.

**(8)** We use `flattenTree` because flakes feature insists on having a flat attribute set of derivations in various places like the `packages` and `checks` attributes. Similar to `legacyPackages`, the `packages` attribute also refer to our flake project. The differences are `packages` build one attribute at the time, only accept one value which is derivation, and it doesn't recurse. That is why we use `flattenTree` to filters out value that is not a derivation, and make it a single `attributeSet`.

**(9)** The `devShell` attribute is self-explanatory. Yes, it is used whenever we want to enter into nix's shell using either `nix shell` or `nix develop`.

**(10)** You can ignore it for now, basically `checks` is used for testing purpose.

----------------------------------

## **| overlay.nix |**
[[Back to the Table of Contents] ↑](#structure)

Basically, `overlay.nix` is a function that return set of derivation. We can see in the following snippet:

```
{ polysemy, http-media, servant, miso, servant-jsaddle }:
final: prev:
let
  ...
in
{
  todomvc = rec {
    ...
    nix = prev.callPackage ./nix { };
    rust-backend = prev.naersk.buildPackage {
      src = ./rust/backend;
      remapPathPrefix = true;
      rustc = nix.rust-overlay;
      cargo = nix.rust-overlay;
    };
  };
}
```

In this project, it takes 3 arguments, `{polysemy, ...}`, `final`, and `prev`. Simply put, we put all of packages that we have in the `inputs` attribute. This means, it is used for third-party package that lives outside our `nixpkgs` environment. While `final` and `prev` is actually `self` and `super` that you might find the best explanation in [here](https://nixos.wiki/wiki/Overlays). As a note, use `prev` for every `nixpkgs` function until you get an error, then you can use `final`.

--------------------------

## **| nix |**
[[Back to the Table of Contents] ↑](#structure)

In todomvc-nix, we try to separate `.nix` file from the packages' folder. So we create `nix` folder that contains a `default.nix`, and each folder representing all packages' name or dependencies. If you refer to the `default.nix` inside `nix` folder, you can see that we just use `callPackage` on every package or dependencies.

```
{ pkgs }:
{
  haskellBackend = pkgs.callPackage ./haskell { };
  haskellMiso = pkgs.callPackage ./haskell-miso { };
  rustOverlay = pkgs.callPackage ./rust-overlay { };
  rustFrontend = pkgs.callPackage ./rust-frontend { };
  database = pkgs.callPackage ./database { };
}
```

By looking at the code, you can see there are sub-directory called `haskell/`, `haskell-miso/`, `rust-overlay/`, `rust-frontend/`, and `database/` which will have at least one `default.nix`.

---------------------------

### **| database |**
[[Back to the Table of Contents] ↑](#structure)

Database's `default.nix` has similar functionality with `nix`'s default.nix. It only calls pgutil.nix and migrate.nix so it is available inside `database` attribute.
```
{ pkgs }:
{
    pgutil = pkgs.callPackage ./pgutil.nix {};
    migrate = pkgs.callPackage ./migrate.nix {};
}
```
-----------
#### **| migrate.nix |**
[[Back to the Table of Contents] ↑](#structure)

We use `sqitch` for database migration. In this file we wrap `sqitch` program to detect the configuration.

```
{ symlinkJoin, sqitchPg, makeWrapper, postgresql }:
symlinkJoin {
  name = "migrate";
  buildInputs = [ makeWrapper ];
  paths = [ sqitchPg ./sql ];
  postBuild = ''
    wrapProgram "$out/bin/sqitch" \
      --add-flags "--chdir $out" \
      --set SQITCH_CONFIG $out/sqitch.conf
  '';
}
```

#### **| pgutil.nix |**
[[Back to the Table of Contents] ↑](#structure)

In this file, we create `start_pg`, `init_pg`, and `stop_pg` scripts to deal with PostgreSQL database.


### **| haskell |**
[[Back to the Table of Contents] ↑](#structure)

This folder is intended to buid todomvc-nix's backend written in Haskell programming language. If you look into project's `overlay.nix`, you will see that we create our own haskell's packages set called `todoHaskellPackages`. In this attribute, we extend the `ghc8102` (means ghc version 8.10.2) packages' set to have our own package. After that, we use `todoHaskellPackages` function called `developPackage` to build Haskell project.

```
todoHaskellPackages.developPackage {
    root = ../../haskell/backend;
    name = "todo-haskell";
    source-overrides = {
        polysemy = todomvc.polysemy;
        ...
        servant = todomvc.servant + "/servant";
        servant-server = todomvc.servant + "/servant-server";
    };
    overrides = self: super: {
        time-compat = fast super.time-compat;
        todo-common = fast super.todo-common;
    };
}
```

Since the project location is outside `nix` folder, then we have to change the project path in the `root` attribute. We add custom packages into `source-overrides` and `overrides` attribute. You can build the backend by running:

```
$ nix build .#nix.haskellBackend
```

or

```
$ nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.haskellBackend
```

--------------------------

### **| haskell-miso |**
[[Back to the Table of Contents] ↑](#structure)

Miso is one of Haskell frontend library. It uses `ghcjs` to transpile Haskell code into Javascript. In todomvc-nix, we extend the `ghcjs` package similar to what we did in the haskell's default.nix. We override the cabal hashes so library like `sevrant-jsaddle` and `polysemy` can be detected when we build the project.

You can build the backend by running:

```
$ nix build .#nix.haskellMiso
```

or

```
$ nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.haskellMiso
```

-----------------

### **| rust-overlay |**
[[Back to the Table of Contents] ↑](#structure)

Rust overlay is a rust tooling in the `mozilla-overlay` derivation. We specifically target `rustc` and `cargo` version based on `rustChannelOf` attribute. Not only `rustc` and `cargo`, we also add some extension in rust attribute as follows:

```
{ rustChannelOf }:
(rustChannelOf {
  channel = "stable";
  sha256 = "sha256-7zt+rHZxx+ha4P/UnT2aNIuBtjPkejVI2PycAt+Apiw=";
}).rust.override {
  extensions = [
    "clippy-preview"
    "rls-preview"
    "rustfmt-preview"
    "rust-analysis"
    "rust-std"
    "rust-src"
  ];
  targets = [ "wasm32-unknown-unknown" ];
}
```

The `targets` attribute is similar to what `cargo`'s target options, in this case we have `WASM` target. Thus, when you use this overlay in nix shell environment, you will get both `rustc` and `cargo` along with its extensions.

You can build the overlay by running:

```
$ nix build .#nix.haskellOverlay
```

or

```
$ nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.haskellOverlay
```

### **| rust-frontend |**
[[Back to the Table of Contents] ↑](#structure)

Rust frontend is built using `yarn`. We use `yarn2nix` and `mkYarnPackage` to build this project.

You can build the frontend by running:

```
$ nix build .#nix.rustFrontend
```

or

```
$ nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.rustFrontend
```
------------------

## **| devshell.nix |**
[[Back to the Table of Contents] ↑](#structure)

This file is used in the `devShell` attribute. We use Numtide's `mkDevShell` function which is similar to `mkShell` function. When we enter the shell environment using `nix develop` every package listed in `packages` attribute will be available in our `PATH` environment.

We can also create several script and put it in `commands` attribute as well as adding environment variable inside `env` attribute.


You can enter into nix shell environment by running:

```
$ nix develop
```

or

```
$ nix-shell
```
