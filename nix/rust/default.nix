{ rustChannelOf }:
(rustChannelOf {
  channel = "stable";
  sha256 = "sha256-adrApiScGGt4lZvGpEjbWiY5e10juOVRhyNpRSLTccI=";
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
