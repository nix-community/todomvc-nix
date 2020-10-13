import rust from "@wasm-tool/rollup-plugin-rust";
import livereload from "rollup-plugin-livereload";
import serve from "rollup-plugin-serve";

export default {
    input: "./Cargo.toml",
    output: {
        dir: "dist/js/",
        format: "iife",
        sourcemap: true,
    },
    plugins: [

        rust({
            serverPath: "js/",
            debug: true,
            watchPatterns: ["src/**"],
            cargoArgs: ["--features", "local quiet"],
            watch: true,
        }),

        serve({
            contentBase: "dist",
            open: true,
            historyApiFallback: true,
        }),

        livereload("dist"),
    ],
};
