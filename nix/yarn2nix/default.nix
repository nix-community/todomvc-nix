{ fetchFromGitHub, callPackage }:
callPackage (fetchFromGitHub {
  owner = "zimbatm";
  repo = "yarn2nix";
  rev = "shell-hook";
  sha256 = "0519ralpcr6307ma9dcjz49n1ahgwh4gr8h4q06hngwvak9cjlh5";
}) {}
