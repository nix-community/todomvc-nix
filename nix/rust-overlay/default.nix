{ rustChannelOf }:
(rustChannelOf {
  channel = "stable";
  sha256 = "sha256-+EFKtTDUlFY0aUXdSvrz7tAhf5/GsqyVOp8skXGTEJM=";
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
