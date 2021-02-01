{ rustChannelOf }:
(rustChannelOf {
  channel = "stable";
  sha256 = "sha256-KCh2UBGtdlBJ/4UOqZlxUtcyefv7MH1neoVNV4z0nWs=";
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
