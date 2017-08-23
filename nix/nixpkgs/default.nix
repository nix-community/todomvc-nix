# This version doesn't depend on another nixpkgs instance
let
  repo = builtins.fromJSON (builtins.readFile ./src.json);
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/${repo.owner}/${repo.repo}/tarball/${repo.rev}";
  };
in import nixpkgs
