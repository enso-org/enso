import path, { dirname } from 'node:path'
import child_process from 'node:child_process'
import { fileURLToPath } from 'node:url'

import webpack from 'webpack'
import CompressionPlugin from 'compression-webpack-plugin'
import CopyWebpackPlugin from 'copy-webpack-plugin'
import NodePolyfillPlugin from 'node-polyfill-webpack-plugin'

import { require_env } from '../../utils.mjs'
import * as BUILD_INFO from '../../build.json' assert { type: 'json' }

const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))

function git(command) {
    return child_process.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
}

const output_path = require_env('ENSO_BUILD_GUI')
const wasm_path = require_env('ENSO_BUILD_GUI_WASM')
const js_glue_path = require_env('ENSO_BUILD_GUI_JS_GLUE')
const assets_path = require_env('ENSO_BUILD_GUI_ASSETS')

// scala-parser.js is compiled from Scala code, so no source map is available for it.
const IGNORE_SOURCE_MAPS = [/scala-parser\.js/]

// Load source maps for JS and TS files, so we will have an accurate source available in DevTools.
// `ignored` is a list of regexes that are matched against file URL to ignore missing source
// maps for certain files.
const sourceMapLoader = ignored => ({
    loader: 'source-map-loader',
    options: {
        filterSourceMappingUrl: (url, _resourcePath) => {
            for (let regexp of ignored) {
                if (regexp.test(url)) return 'skip'
            }
            return true
        },
    },
})

export default {
    entry: {
        index: path.resolve(thisPath, 'src', 'index.ts'),
        wasm_imports: './src/wasm_imports.js',
    },
    output: {
        path: path.resolve(output_path, 'assets'),
        publicPath: '/assets/',
        filename: '[name].js',
        libraryTarget: 'umd',
    },
    plugins: [
        new CompressionPlugin(),
        new CopyWebpackPlugin({
            patterns: [
                path.resolve(thisPath, 'src', 'index.html'),
                path.resolve(thisPath, 'src', 'run.js'),
                path.resolve(thisPath, 'src', 'style.css'),
                path.resolve(thisPath, 'src', 'docsStyle.css'),
                assets_path,
                wasm_path,
            ],
            options: {},
        }),
        new webpack.DefinePlugin({
            GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
            GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
            BUILD_INFO: JSON.stringify(BUILD_INFO),
        }),
        new NodePolyfillPlugin({}),
    ],
    devtool: 'eval-source-map',
    devServer: {
        historyApiFallback: {
            index: '/assets/',
        },
    },
    resolve: {
        alias: {
            wasm_rust_glue$: js_glue_path,
        },
        extensions: ['.ts', '.js'],
        fallback: {
            fs: false,
        },
    },
    performance: {
        hints: false,
    },
    mode: 'none',
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
            Object.assign(
                {
                    test: [/\.js$/, /\.tsx?$/],
                    enforce: 'pre',
                    exclude: /node_modules[\\\/]@firebase/,
                },
                sourceMapLoader(IGNORE_SOURCE_MAPS)
            ),
        ],
    },
}
