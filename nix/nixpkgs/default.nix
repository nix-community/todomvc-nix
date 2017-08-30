# Pinned nixpkgs.
#
# This isn't using fetchFromGitHub or anything fancy because we don't have
# access to nixpkgs yet. We could use <nixpkgs> but then it doesn't work
# when using NIX_PATH. So we are stuck with using built-ins.
#
# It's also avoiding the builtins.fetchurl / fetchTarball because they fetch
# the source on every call. Until nix 1.12 is released these function don't support the
# necessary sha256 attributes.
#
# To simplify the updating of the source, an ./update script in the same folder exists that
# should be used to do so.
#
# So here is a convoluted version that uses a lot of nix internals, credit goes to @taktoa:
# https://gist.github.com/taktoa/4ec8b0cb6d07857400b07a830ad99558
let
  cfg = import <nix/config.nix>;
  name = "${repo.repo}-${repo.branch}";
  repo = builtins.fromJSON (builtins.readFile ./src.json);
  system = builtins.currentSystem;

  tarball = import <nix/fetchurl.nix> {
    inherit system;
    url = "https://github.com/${repo.owner}/${repo.repo}/archive/${repo.rev}.tar.gz";
    sha256 = repo.sha256;
    executable = false;
    unpack = false; # unpack only works on NAR files
  };

  src = builtins.derivation {
    inherit name system tarball;
    builder = cfg.shell;
    args = [
      (builtins.toFile "unpack" ''
        "$coreutils/mkdir" "$out"
        "$gzip" -c -d "$tarball" | "$tar" -x -C "$out" --strip-components=1
      '')
    ];

    inherit (cfg) tar gzip coreutils;
  };
in
  import src
