/**
 * Configuration for the esbuild bundler and build/watch commands.
 *
 * The bundler processes each entry point into a single file, each with no external dependencies and
 * minified. This primarily involves resolving all imports, along with some other transformations
 * (like TypeScript compilation).
 *
 * See the bundlers documentation for more information:
 * https://esbuild.github.io/getting-started/#bundling-for-node
 */

import fs from 'node:fs'
import path, { dirname } from 'node:path'
import child_process from 'node:child_process'
import { fileURLToPath } from 'node:url'

import esbuild from 'esbuild'
import plugin_yaml from 'esbuild-plugin-yaml'
import { NodeModulesPolyfillPlugin } from '@esbuild-plugins/node-modules-polyfill'
import { NodeGlobalsPolyfillPlugin } from '@esbuild-plugins/node-globals-polyfill'
import aliasPlugin from 'esbuild-plugin-alias'
// @ts-ignore
import timePlugin from 'esbuild-plugin-time'
// @ts-ignore
import * as copy_plugin from 'enso-copy-plugin'

import { require_env } from '../../utils.js'
import * as BUILD_INFO from '../../build.json' assert { type: 'json' }

export const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))

// =============================
// === Environment variables ===
// =============================

/** List of files to be copied from WASM artifacts. */
export const wasm_artifacts = require_env('ENSO_BUILD_GUI_WASM_ARTIFACTS')

/** Directory with assets. Its contents are to be copied. */
export const assets_path = require_env('ENSO_BUILD_GUI_ASSETS')

/** Path where bundled files are output. */
export const output_path = path.resolve(require_env('ENSO_BUILD_GUI'), 'assets')

/** The main JS bundle to load WASM and JS wasm-pack bundles. */
export const ensogl_app_path = require_env('ENSO_BUILD_GUI_ENSOGL_APP')

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
    //            The bundler configuration should not invoke git, it is not its responsibility.
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
    ...wasm_artifacts.split(path.delimiter),
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
    entryPoints: ['src/index.ts'],
    outdir: output_path,
    outbase: 'src',
    plugins: [
        plugin_yaml.yamlPlugin({}),
        NodeModulesPolyfillPlugin(),
        NodeGlobalsPolyfillPlugin({ buffer: true, process: true }),
        aliasPlugin({ ensogl_app: ensogl_app_path }),
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
    platform: 'node',
    incremental: true,
    color: true,
    logOverride: {
        // Happens in ScalaJS-generated parser (scala-parser.js):
        //    6 │   "fileLevelThis": this
        'this-is-undefined-in-esm': 'silent',
        // Happens in ScalaJS-generated parser (scala-parser.js):
        // 1553 │   } else if ((a === (-0))) {
        'equals-negative-zero': 'silent',
        // Happens in Emscripten-generated MSDF (msdfgen_wasm.js):
        //    1 │ ...typeof module!=="undefined"){module["exports"]=Module}process["o...
        'commonjs-variable-in-esm': 'silent',
        // Happens in Emscripten-generated MSDF (msdfgen_wasm.js):
        //    1 │ ...y{table.grow(1)}catch(err){if(!err instanceof RangeError){throw ...
        'suspicious-boolean-not': 'silent',
    },
}

/**
 * Spawn the esbuild watch process. It continuously runs, rebuilding the package.
 */
export async function watch(onRebuild?: () => void, inject?: esbuild.BuildOptions['inject']) {
    return esbuild.build({
        ...config,
        inject: [...(config.inject ?? []), ...(inject ?? [])],
        watch: {
            onRebuild(error, result) {
                if (error) console.error('watch build failed:', error)
                else onRebuild?.()
            },
        },
    })
}

/**
 * Bundles the package.
 */
export async function bundle() {
    return esbuild.build({ ...config, watch: false, incremental: false })
}

export default { watch, bundle, output_path }
