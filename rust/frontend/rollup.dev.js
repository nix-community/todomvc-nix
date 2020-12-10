import rust from "@wasm-tool/rollup-plugin-rust";
import livereload from "rollup-plugin-livereload";
import serve from "rollup-plugin-serve";

export default {
    input: {
        index: 'Cargo.toml'
    },
    output: {
        dir: "devhtml/js",
        format: "iife",
        chunkFileNames: "[name].js",
        assetFileNames: "assets/[name][extname]",
        sourcemap: true
    },
    plugins: [
        rust({
            serverPath: "/js/",
            debug: true,
            verbose: true,
            watchPatterns: ["src/**"],
            cargoArgs: ["--features", "develop"],
        }),

        serve({
            contentBase: 'devhtml',
            open: true,
            verbose: true,
            // historyApiFallback: true,
        }),

        livereload({
            watch: 'devhtml/js',
            verbose: true

        })
    ],
    watch: {
        clearScreen: false
    }
};
