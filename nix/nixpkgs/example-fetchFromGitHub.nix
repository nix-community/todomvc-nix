# Example with recursive nixpkgs and fetchFromGitHub
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
in
  fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "daf53c9a102e695d9ca0c2b48b231e8f2517264b";
    sha256 = "1dsszs9ds17n006yv4il39640msslmhdh3ckg4l0vfq8y11ljvss";
  }
