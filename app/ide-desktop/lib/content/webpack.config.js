const CopyWebpackPlugin = require('copy-webpack-plugin')
const CompressionPlugin = require('compression-webpack-plugin')
const path = require('path')
const webpack = require('webpack')

const thisPath = path.resolve(__dirname)
const root = path.resolve(thisPath, '..', '..', '..', '..')
const distPath = path.resolve(root, 'dist')
const wasmPath = path.resolve(distPath, 'wasm')
const buildPath = path.resolve(distPath, 'build.json')

const child_process = require('child_process')
function git(command) {
    return child_process.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
}

const BUILD_INFO = JSON.parse(require('fs').readFileSync(buildPath, 'utf8'))

// This loader removes all `sourceMappingURL` comments from the source files.
//
// We do not generate and do not bundle source maps for JS files at the moment, but some of our dependencies
// have `sourceMappingURL` in their minified code (notably `firebase`). When DevTools load our bundled JS file,
// they print a warning about missing source maps. To avoid this warning, we remove all `sourceMappingURL` from
// our codebase.
//
// See https://blog.teamtreehouse.com/introduction-source-maps for basic introduction to `sourceMappingURL`.
// See https://webpack.js.org/configuration/devtool/ for information on how to enable source map generation.
const sourceMapLoader = {
    loader: 'source-map-loader',
    options: {
        filterSourceMappingUrl: (_url, _resourcePath) => 'remove',
    },
}

module.exports = {
    entry: {
        index: path.resolve(thisPath, 'src', 'index.ts'),
        wasm_imports: './src/wasm_imports.js',
    },
    output: {
        path: path.resolve(root, 'dist', 'content', 'assets'),
        filename: '[name].js',
        libraryTarget: 'umd',
    },
    node: {
        fs: 'empty',
    },
    plugins: [
        new CompressionPlugin(),
        new CopyWebpackPlugin([
            path.resolve(thisPath, 'src', 'index.html'),
            path.resolve(thisPath, 'src', 'run.js'),
            path.resolve(thisPath, 'src', 'style.css'),
            path.resolve(thisPath, 'src', 'docsStyle.css'),
            path.resolve(thisPath, 'assets'),
            path.resolve(wasmPath, 'ide.wasm'),
        ]),
        new webpack.DefinePlugin({
            GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
            GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
            BUILD_INFO: JSON.stringify(BUILD_INFO),
            FIREBASE_API_KEY: JSON.stringify(process.env.FIREBASE_API_KEY),
        }),
    ],
    devServer: {
        publicPath: '/assets/',
        historyApiFallback: {
            index: '/assets/',
        },
    },
    resolve: {
        alias: {
            wasm_rust_glue$: path.resolve(wasmPath, 'ide.js'),
        },
        extensions: ['.ts', '.js'],
    },
    performance: {
        hints: false,
    },
    mode: 'none',
    stats: 'minimal',
    module: {
        rules: [
            {
                test: /\.ya?ml$/,
                type: 'json',
                use: 'yaml-loader',
            },
            {
                test: /\.tsx?/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
            {
                test: /\.html$/i,
                loader: 'html-loader',
            },
            {
                test: /\.js$/,
                enforce: 'pre',
                loader: sourceMapLoader,
            },
        ],
    },
}
