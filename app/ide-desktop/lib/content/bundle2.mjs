import esbuild from 'esbuild'
import 'esbuild-plugin-yaml'
import plugin_yaml from 'esbuild-plugin-yaml'
import NodeModulesPolyfillPlugin from '@esbuild-plugins/node-modules-polyfill'
import path, { dirname } from 'node:path'
import child_process from 'node:child_process'
import { fileURLToPath } from 'node:url'
import aliasPlugin from 'esbuild-plugin-alias'
import timePlugin from 'esbuild-plugin-time'
import copyPlugin from 'esbuild-copy-static-files'
import { require_env } from '../../utils.mjs'
import * as BUILD_INFO from '../../build.json' assert { type: 'json' }
import * as fs from "fs";

import esBuildDevServer from "esbuild-dev-server";

const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))

function git(command) {
    return child_process.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
}

const output_path = require_env('ENSO_BUILD_GUI')
const wasm_path = require_env('ENSO_BUILD_GUI_WASM')
const js_glue_path = require_env('ENSO_BUILD_GUI_JS_GLUE')
const assets_path = require_env('ENSO_BUILD_GUI_ASSETS')

function sleep(ms) {
    return new Promise(resolve => {
        setTimeout(resolve, ms)
    })
}

// await sleep(100000);

let files_to_copy = [
    path.resolve(thisPath, 'src', 'index.html'),
    path.resolve(thisPath, 'src', 'run.js'),
    path.resolve(thisPath, 'src', 'style.css'),
    path.resolve(thisPath, 'src', 'docsStyle.css'),
    wasm_path,
]

let my_copy_plugin = {
    name: 'mwu-copy',
    setup(build) {
        console.log(build)
        build.onEnd(() => {
            const cpOpts = {
                recursive: true,
                force: true,
                dereference: true
            };
            fs.cpSync(assets_path, build.initialOptions.outdir, cpOpts)
            files_to_copy.forEach(from => fs.cpSync(from, path.join(build.initialOptions.outdir, path.basename(from)), cpOpts))
        });
    },
};

let config = {
    bundle: true,
    entryPoints: ['src/index.ts', 'src/wasm_imports.js'],
    outdir: 'assets',
    plugins: [
        plugin_yaml.yamlPlugin(),
        NodeModulesPolyfillPlugin.NodeModulesPolyfillPlugin(),
        aliasPlugin({ wasm_rust_glue: js_glue_path }),
        timePlugin(),
        my_copy_plugin
    ],
    define: {
        GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
        GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
        BUILD_INFO: JSON.stringify(BUILD_INFO),
    },
    sourcemap: true,
    minify: true,
    metafile: true,
    publicPath: '/assets',
    incremental: true,
}

let buildJob = esbuild.build(config)

esBuildDevServer.start(
    buildJob,
    {
        port:      "8080", // optional, default: 8080
        watchDir:  "src", // optional, default: "src"
        index:     "assets/index.html", // optional
        staticDir: "assets", // optional
        onBeforeRebuild: {}, // optional
        onAfterRebuild:  {}, // optional
    }
)


//
// await esbuild.serve({
//     servedir: 'assets',
// }, config)
//
// let result = await buildJob




// let text = await esbuild.analyzeMetafile(result.metafile)
// console.log(text)
