let
  spec = builtins.fromJSON (builtins.readFile ./nixpkgs-src.json);
in
  fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
  }
