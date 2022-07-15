/**
 * Configuration for the esbuild bundler and build/watch commands.
 */

import fs from 'node:fs'
import path, { dirname } from 'node:path'
import child_process from 'node:child_process'
import { fileURLToPath } from 'node:url'

import esbuild from 'esbuild'
import plugin_yaml from 'esbuild-plugin-yaml'
import { NodeModulesPolyfillPlugin } from '@esbuild-plugins/node-modules-polyfill'
import aliasPlugin from 'esbuild-plugin-alias'
// @ts-ignore
import timePlugin from 'esbuild-plugin-time'
// @ts-ignore
import * as copy_plugin from 'enso-copy-plugin'

import { require_env } from '../../utils.js'
import * as BUILD_INFO from '../../build.json' assert { type: 'json' }

// =============================
// === Environment variables ===
// =============================

export const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))
/** Path where bundled files are output. */
export const output_path = path.resolve(require_env('ENSO_BUILD_GUI'), 'assets')
export const wasm_path = require_env('ENSO_BUILD_GUI_WASM')
export const js_glue_path = require_env('ENSO_BUILD_GUI_JS_GLUE')
export const assets_path = require_env('ENSO_BUILD_GUI_ASSETS')

// ===================
// === Git process ===
// ===================

/**
 * Get output of a git command.
 * @param command Command line following the `git` program.
 * @returns Output of the command.
 */
function git(command: string): string {
    // TODO [mwu] Eventually this should be removed, data should be provided by the build script through `BUILD_INFO`.
    return child_process.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
}

// ==============================
// === Files to manually copy ===
// ==============================

/**
 * Static set of files that are always copied to the output directory.
 */
const always_copied_files = [
    path.resolve(thisPath, 'src', 'index.html'),
    path.resolve(thisPath, 'src', 'run.js'),
    path.resolve(thisPath, 'src', 'style.css'),
    path.resolve(thisPath, 'src', 'docsStyle.css'),
    wasm_path,
]

/**
 * Generator that yields all files that should be copied to the output directory.
 */
async function* files_to_copy_provider() {
    console.log('Preparing a new generator for files to copy.')
    yield* always_copied_files
    for (const file of await fs.promises.readdir(assets_path)) {
        yield path.resolve(assets_path, file)
    }
    console.log('Generator for files to copy finished.')
}

// ================
// === Bundling ===
// ================

const config: esbuild.BuildOptions = {
    bundle: true,
    entryPoints: ['src/index.ts', 'src/wasm_imports.js'],
    outdir: output_path,
    outbase: 'src',
    plugins: [
        plugin_yaml.yamlPlugin({}),
        NodeModulesPolyfillPlugin(),
        aliasPlugin({ wasm_rust_glue: js_glue_path }),
        timePlugin(),
        copy_plugin.create(files_to_copy_provider),
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
    color: true,
    watch: {
        onRebuild(error, result) {
            if (error) console.error('watch build failed:', error)
            // else console.log('watch build succeeded:', result)
        },
    },
}

/**
 * Spawn the esbuild watch process. It continuously runs, rebuilding the package.
 */
export async function watch() {
    return esbuild.build(config)
}

/**
 * Bundles the package.
 */
export async function bundle() {
    return esbuild.build({ ...config, watch: false, incremental: false })
}

export default { watch, bundle, output_path }
