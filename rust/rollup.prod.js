import rust from "@wasm-tool/rollup-plugin-rust";

export default {
    input: {
        index: "Cargo.toml",
    },
    output: {
        dir: "dist/js",
        format: "iife",
        sourcemap: true,
        chunkFileNames: "[name]-[hash].js",
        assetFileNames: "assets/[name]-[hash].[extname]",
    },
    plugins: [
        rust({
            serverPath: "/js/",
            debug: false,
            verbose: true
        }),
    ],
};
