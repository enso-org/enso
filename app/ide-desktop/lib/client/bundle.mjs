import path, { dirname } from 'node:path'
import { fileURLToPath } from 'node:url'

import esbuild from 'esbuild'
import NodeModulesPolyfillPlugin from '@esbuild-plugins/node-modules-polyfill'

import { require_env } from '../../utils.mjs'
import * as copy_plugin from "enso-copy-plugin";
// import * as BUILD_INFO from "../../build.json" assert { type: 'json' };
// import {js_glue_path} from "enso-studio-content/bundle2.mjs";

const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))
const dist = path.resolve(require_env('ENSO_BUILD_IDE'), 'client')
const project_manager_in_bundle_path = require_env('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')
const bundled_engine_version = process.env['ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION']

const entryPoints = ['src/index.js', 'src/preload.cjs']
const define = {
    BUNDLED_ENGINE_VERSION: JSON.stringify(bundled_engine_version),
    PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(project_manager_in_bundle_path),
}

function filesToCopy() {
    return [
        path.resolve(thisPath, 'package.json')
    ]
}

const options = {
    bundle: true,
    outdir: dist,
    outbase: 'src',
    platform: 'node',
    plugins: [
        // plugin_yaml.yamlPlugin(),
        NodeModulesPolyfillPlugin.NodeModulesPolyfillPlugin(),
        // aliasPlugin({ wasm_rust_glue: js_glue_path }),
        // timePlugin(),
        // copy_plugin.create(files_to_copy_provider)
    ],
    entryPoints,
    define,
    external: ['emitter'] // required only for non-node environment by batch
}
await esbuild.build(options)

// import { createRequire } from 'node:module'
//
// import webpack from 'webpack'
// import CopyPlugin from 'copy-webpack-plugin'
//
// const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))
// const dist = path.resolve(require_env('ENSO_BUILD_IDE'), 'client')
// const project_manager_in_bundle_path = require_env('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')
// const bundled_engine_version = process.env['ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION']
//
// export default {
//     entry: {
//         index: path.resolve(thisPath, 'src', 'index.js'),
//         preload: path.resolve(thisPath, 'src', 'preload.cjs'),
//     },
//     mode: 'production',
//     target: 'electron-main',
//     output: {
//         path: dist,
//         filename: '[name].cjs',
//     },
//     plugins: [
//         new CopyPlugin({
//             patterns: [
//                 {
//                     from: path.resolve(thisPath, 'package.json'),
//                     to: path.join(dist, 'package.json'),
//                 },
//             ],
//             options: {},
//         }),
//         new webpack.DefinePlugin({
//             BUNDLED_ENGINE_VERSION: JSON.stringify(bundled_engine_version),
//             PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(project_manager_in_bundle_path),
//         }),
//     ],
//     performance: {
//         hints: false,
//     },
//     stats: 'minimal',
// }
//
//

// import esbuild from 'esbuild'
// import 'esbuild-plugin-yaml'
// import plugin_yaml from 'esbuild-plugin-yaml'
// import NodeModulesPolyfillPlugin from '@esbuild-plugins/node-modules-polyfill'
// import path, {dirname} from 'node:path'
// import child_process from 'node:child_process'
// import {fileURLToPath} from 'node:url'
// import aliasPlugin from 'esbuild-plugin-alias'
// import timePlugin from 'esbuild-plugin-time'
// import * as BUILD_INFO from '../../build.json' assert {type: 'json'}
// import * as fs from "fs";
// import * as copy_plugin from 'enso-copy-plugin'
//
// export const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))
// export const output_path = path.resolve(require_env('ENSO_BUILD_GUI'), 'assets')
// export const wasm_path = require_env('ENSO_BUILD_GUI_WASM')
// export const js_glue_path = require_env('ENSO_BUILD_GUI_JS_GLUE')
// export const assets_path = require_env('ENSO_BUILD_GUI_ASSETS')
//
// function git(command) {
//     return child_process.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
// }
//
// const always_copied_files = [
//     path.resolve(thisPath, 'src', 'index.html'),
//     path.resolve(thisPath, 'src', 'run.js'),
//     path.resolve(thisPath, 'src', 'style.css'),
//     path.resolve(thisPath, 'src', 'docsStyle.css'),
//     wasm_path,
// ]
//
// async function* files_to_copy_provider() {
//     console.log("Preparing a new generator for files to copy.")
//     yield* always_copied_files
//     for (const file of await fs.promises.readdir(assets_path)) {
//         yield path.resolve(assets_path, file)
//     }
//     console.log("Generator for files to copy finished.")
// }
//
// const config = {
//     bundle: true,
//     entryPoints: ['src/index.ts', 'src/wasm_imports.js'],
//     outdir: output_path,
//     outbase: 'src',
//     plugins: [
//         plugin_yaml.yamlPlugin(),
//         NodeModulesPolyfillPlugin.NodeModulesPolyfillPlugin(),
//         aliasPlugin({ wasm_rust_glue: js_glue_path }),
//         timePlugin(),
//         copy_plugin.create(files_to_copy_provider)
//     ],
//     define: {
//         GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
//         GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
//         BUILD_INFO: JSON.stringify(BUILD_INFO),
//     },
//     sourcemap: true,
//     minify: true,
//     metafile: true,
//     publicPath: '/assets',
//     incremental: true,
//     color: true,
//     watch: {
//         onRebuild(error, result) {
//             if (error) console.error('watch build failed:', error)
//             else console.log('watch build succeeded:', result)
//         },
//     },
// }
//
// export async function bundle() {
//     return esbuild.build(config)
// }
