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
