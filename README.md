# TodoMVC-Nix
#### One-stop reference to build TodoMVC application inside the Nix world
### [![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Are you new to Nix? Or are you use Nix, but still don't know how `Nix` is useful for your company? Maybe you have use Nix/NixOS in your current project, and you want to know how other project (using other programming language) fits into Nix world? TodoMVC-Nix can help you to understand how your project is structured using Nix to ease your project development setting.

This project's goal is to provide Nix user on every level (beginner, intermediate, and advance) a reference for building project using Nix programming language new feature called `Flakes`. This project contains several project written in different programming language and also how to nixified it.

Related article: [todomvc-nix: One-Stop Solution for Developing Projects Using Nix](https://numtide.com/articles/todomvc-nix-rejuvenation/)

## Structure
* [Technology Stack](#technology-stack)
* [Get Started](#get-started)
  + [Prerequisites](#prerequisites)
  + [Setup](#setup)
* [Usage](#usage)
* [For Developers](#for-developers)
  + [Build](#build)
  + [Run](#run)
* [Acknowledgements](#acknowledgements)


## Technology Stack
[[Back to the Table of Contents] ↑](#structure)

TodoMVC-Nix has several examples on how fullstack project is structured using Nix Flakes. This stack can be used as an example for early stage startup or some new project which might want to consider to use Nix in their development journey. The stack consists of the following:

* Backend
  + Rust
  + Haskell
* Frontend
  + Rust
  + Haskell
* Database
  + PostgreSQL

Another language and stack will be added accordingly.

## Get Started
[[Back to the Table of Contents] ↑](#structure)

Before exploring the project, there are several things that need to be prepared first as explained below.

### Prerequisites
[[Back to the Table of Contents] ↑](#structure)

Make sure that you have the following package installed in your machine:
  * `nix` (follow [this guide](https://nixos.wiki/wiki/Nix_Installation_Guide))  with flakes enabled
  * `git`

### Setup
[[Back to the Table of Contents] ↑](#structure)

#### Non-nix-flake user

This repository can be used using plain `nix-build` or `nix-shell`. This is possible with the help of `flake-compat` as mentioned in [default.nix](./default.nix). To build the package, just run:

```
$ nix-build -A defaultNix.legacyPackages.x86_64-linux.nix.haskellBackend
```

and to enter into Nix shell, run:

```
$ nix-shell
```

#### Nix-flake user
If you want to use this repo with `flakes` feature, please enable it using the following method:

**Linux and Windows Subsystem Linux 2 (WSL2)**

Install Nix as instructed above. Next, install `nixUnstable` by running the following code:

```
nix-env -iA nixpkgs.nixFlakes
```

Lastly, open your `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` file and add:
```
experimental-features = nix-command flakes
```

**NixOS**

Add the following code into your `configuration.nix`:

```
{ pkgs, ... }: {
  nix = {
    package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
  };
}
```

## Usage
[[Back to the Table of Contents] ↑](#structure)

Clone this repository, and enter into it:

```
$ git clone https://github.com/nix-community/todomvc-nix/tree/flake
$ cd todomvc-nix
```

run `nix develop` and wait until the downloading/caching the dependencies are finished.

Once it is finished, you can refer to the documentation below:
* [Haskell](docs/Haskell.md)
* [Rust](docs/Rust.md)
* [PostgreSQL](docs/PostgreSQL.md)
* [Nix](docs/Nix.md)

## For Developers
[[Back to the Table of Contents] ↑](#structure)

### **Build**
[[Back to the Table of Contents] ↑](#structure)

Buildin package in Nix Flakes is simply by running the following code:

```
nix build .#todomvc.nix.haskellBackend
```

The above example will build `todo-haskell` package which is a backend built in Haskell. Here is the breakdown of the command:
* `nix build` - is a nix v2 command to build package
* `.#todomvc.nix.haskellBackend`
  + `.` -  it means the directory contain `flake.nix`,
  + `#` - it is to separate between directory and package,
  + `todomvc.nix.haskellBackend` - is the package describe in the [overlay](overlay.nix) that we want to build.

The result of the build will be available in the `result` folder.

### **Run**
[[Back to the Table of Contents] ↑](#structure)

By running `nix develop` in the project, some command as written in [shell.nix](shell.nix) will be available in the `PATH`.

There is also custom script that can be created to simplify the command as specified in the `commands` attributes in [shell.nix](shell.nix).

For example, command `pgstart` can be run to start the postgresql server. `pgstart` is custom script based on the following code:

```
{
  name = "pgstart";
  help = "start psql service";
  command = "${todomvc.nix.database.pgutil.start_pg} || echo '''PG start failed''' ";
}
```

Please refer to [shell.nix](shell.nix) for further command and package.

## Acknowledgements
[[Back to the Table of Contents] ↑](#structure)

Most of the code that we put in this repo has previously been made by others. [Here](References.md) we credit the repository that we take the code from.

