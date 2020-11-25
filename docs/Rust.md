# TodoMVC-Nix Rust

After running `nix develop`, there are packages added in the `PATH` for Rust development, like `cargo`, `rustc` and `rustfmt`.

## Development

In Nix, there are couple ways to develop rust package. In this repo, `nixkgs-mozilla` is selected because it is the most up to date rust environment in Nix world.

### **Backend**

Go to backend rust folder and run the cargo command as follows:

```
$ cd backend/rust
$ cargo run
```

### **Frontend**

To develop frontend in Rust, only `yarn` or `npm` is needed to compile Rust into WebAssembly.

Please refer to [package.json](../frontend/rust/package.json) for further reference. In short, `rollupjs` is used to compile/package rust to wasm.
