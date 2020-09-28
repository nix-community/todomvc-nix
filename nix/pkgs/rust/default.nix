{ rustChannelOf }:
(rustChannelOf {
  channel = "stable";
  sha256 = "1sm9g2vkzm8a6w35rrwngnzac28ryszbzwl5y5wrj4qxlmjxw8n5";
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
