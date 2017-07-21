{ fetchFromGitHub, callPackage }:
callPackage (fetchFromGitHub {
  owner = "zimbatm";
  repo = "yarn2nix";
  rev = "refactor";
  sha256 = "1sxxcinwf7a0klhs4zwm2z1g255944dp5r5yslpyf21k3bkri962";
}) {}
