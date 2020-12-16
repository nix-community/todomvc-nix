{ pkgs, binaryen, stdenv, todomvc }:
let
  package = (pkgs.makeRustPlatform {
    rustc = todomvc.nix.rustOverlay;
    cargo = todomvc.nix.rustOverlay;
  }).buildRustPackage rec {
    pname = "rust-frontend";
    version = "0.1.0";
    src = ../../rust;
    cargoSha256 = "sha256-lXnAyJUSWuu3fwR64wPnhWQV2gO4d7HXz+579QEiXnk=";
    doCheck = false;
    cargoBuildFlags = [ "-p" pname ];
    target = "wasm32-unknown-unknown";
  };

in
stdenv.mkDerivation rec {
  name = "wasm-bindgen-cli";
  version = "0.2.69";
  src = fetchTarball {
    url = "https://github.com/rustwasm/wasm-bindgen/releases/download/${version}/wasm-bindgen-${version}-x86_64-unknown-linux-musl.tar.gz";
    sha256 = "1d3s7f41bil5c3m0801nw6a07rq804rfh79wlvpp35s7l0j1y9an";
  };
  buildPhase = ''
    ${src}/wasm-bindgen --target web --out-dir . ${package}/bin/rust_frontend.wasm
    ${binaryen}/bin/wasm-opt -Os rust_frontend_bg.wasm -o rust_frontend_bg.wasm
  '';
  installPhase = ''
    mkdir -p $out
    cp -R rust_* snippets $out/
  '';
}
