import rust from "@wasm-tool/rollup-plugin-rust";

export default {
    input: {
        index: "./Cargo.toml",
    },
    output: {
        dir: "devhtml/js",
        format: "iife",
        sourcemap: true,
    },
    plugins: [
        rust({
            serverPath: "/js/",
            debug: true,
            cargoArgs: ["--features", "local quiet"],
        }),
    ],
};
