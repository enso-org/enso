/**
 * @file Configuration for the esbuild bundler and build/watch commands.
 *
 * The bundler processes each entry point into a single file, each with no external dependencies and
 * minified. This primarily involves resolving all imports, along with some other transformations
 * (like TypeScript compilation).
 *
 * See the bundlers documentation for more information:
 * https://esbuild.github.io/getting-started/#bundling-for-node.
 */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import * as esbuildPluginCopy from 'enso-copy-plugin'
import * as esbuildPluginNodeGlobals from '@esbuild-plugins/node-globals-polyfill'
import * as esbuildPluginNodeModules from '@esbuild-plugins/node-modules-polyfill'
import esbuildPluginAlias from 'esbuild-plugin-alias'
import esbuildPluginTime from 'esbuild-plugin-time'
import esbuildPluginYaml from 'esbuild-plugin-yaml'

import * as esbuildWatch from '../../esbuild-watch.js'
import * as utils from '../../utils.js'
import BUILD_INFO from '../../build.json' assert { type: 'json' }

export const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// =================
// === Constants ===
// =================

const TAILWIND_BINARY_PATH = '../../node_modules/.bin/tailwindcss'
const TAILWIND_CSS_PATH = path.resolve(THIS_PATH, 'src', 'tailwind.css')

// =============================
// === Environment variables ===
// =============================

export interface Arguments {
    /** List of files to be copied from WASM artifacts. */
    wasmArtifacts: string
    /** Directory with assets. Its contents are to be copied. */
    assetsPath: string
    /** Path where bundled files are output. */
    outputPath: string
    /** The main JS bundle to load WASM and JS wasm-pack bundles. */
    ensoglAppPath: string
}

/**
 * Get arguments from the environment.
 */
export function argumentsFromEnv(): Arguments {
    const wasmArtifacts = utils.requireEnv('ENSO_BUILD_GUI_WASM_ARTIFACTS')
    const assetsPath = utils.requireEnv('ENSO_BUILD_GUI_ASSETS')
    const outputPath = path.resolve(utils.requireEnv('ENSO_BUILD_GUI'), 'assets')
    const ensoglAppPath = utils.requireEnv('ENSO_BUILD_GUI_ENSOGL_APP')
    return { wasmArtifacts, assetsPath, outputPath, ensoglAppPath }
}

// ===================
// === Git process ===
// ===================

/**
 * Get output of a git command.
 * @param command - Command line following the `git` program.
 * @returns Output of the command.
 */
function git(command: string): string {
    // TODO [mwu] Eventually this should be removed, data should be provided by the build script through `BUILD_INFO`.
    //            The bundler configuration should not invoke git, it is not its responsibility.
    return childProcess.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
}

// ==============================
// === Files to manually copy ===
// ==============================

/**
 * Static set of files that are always copied to the output directory.
 */
export function alwaysCopiedFiles(wasmArtifacts: string) {
    return [
        path.resolve(THIS_PATH, 'src', 'index.html'),
        path.resolve(THIS_PATH, 'src', 'run.js'),
        path.resolve(THIS_PATH, 'src', 'style.css'),
        path.resolve(THIS_PATH, 'src', 'docsStyle.css'),
        path.resolve(THIS_PATH, 'src', 'tailwind.css'),
        ...wasmArtifacts.split(path.delimiter),
    ]
}

/**
 * Generator that yields all files that should be copied to the output directory.
 * @yields {string} The file path of the next file to be copied.
 */
export async function* filesToCopyProvider(wasmArtifacts: string, assetsPath: string) {
    console.log('Preparing a new generator for files to copy.')
    yield* alwaysCopiedFiles(wasmArtifacts)
    for (const file of await fs.readdir(assetsPath)) {
        yield path.resolve(assetsPath, file)
    }
    console.log('Generator for files to copy finished.')
}

// ======================
// === Inline plugins ===
// ======================

function esbuildPluginGenerateTailwind(args: Pick<Arguments, 'assetsPath'>): esbuild.Plugin {
    return {
        name: 'enso-generate-tailwind',
        setup: build => {
            // Required since `onStart` is called on every rebuild.
            let firstRun = true
            build.onStart(() => {
                if (firstRun) {
                    const dest = path.join(args.assetsPath, 'tailwind.css')
                    const config = path.resolve(THIS_PATH, 'tailwind.config.ts')
                    console.log(`Generating tailwind css from '${TAILWIND_CSS_PATH}' to '${dest}'.`)
                    const child = childProcess.spawn(`node`, [
                        TAILWIND_BINARY_PATH,
                        '-i',
                        TAILWIND_CSS_PATH,
                        'o',
                        dest,
                        '-c',
                        config,
                        '--minify',
                    ])
                    firstRun = false
                    return new Promise(resolve =>
                        child.on('close', () => {
                            resolve({})
                        })
                    )
                } else {
                    return {}
                }
            })
        },
    }
}

// ================
// === Bundling ===
// ================

/**
 * Generate the builder options.
 */
export function bundlerOptions(args: Arguments) {
    const { outputPath, ensoglAppPath, wasmArtifacts, assetsPath } = args
    // This is required to make the `true` properties be typed as `boolean`.
    // eslint-disable-next-line no-restricted-syntax
    let trueBoolean = true as boolean
    const buildOptions = {
        // Disabling naming convention because these are third-party options.
        /* eslint-disable @typescript-eslint/naming-convention */
        absWorkingDir: THIS_PATH,
        bundle: trueBoolean,
        entryPoints: [path.resolve(THIS_PATH, 'src', 'index.ts')],
        outdir: outputPath,
        outbase: 'src',
        plugins: [
            esbuildPluginYaml.yamlPlugin({}),
            esbuildPluginNodeModules.NodeModulesPolyfillPlugin(),
            esbuildPluginNodeGlobals.NodeGlobalsPolyfillPlugin({ buffer: true, process: true }),
            esbuildPluginAlias({ ensogl_app: ensoglAppPath }),
            esbuildPluginTime(),
            // This must run before the copy plugin so that the generated `tailwind.css` is used.
            esbuildPluginGenerateTailwind({ assetsPath }),
            esbuildPluginCopy.create(() => filesToCopyProvider(wasmArtifacts, assetsPath)),
        ],
        define: {
            GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
            GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
            BUILD_INFO: JSON.stringify(BUILD_INFO),
        },
        sourcemap: trueBoolean,
        minify: trueBoolean,
        metafile: trueBoolean,
        format: 'esm',
        publicPath: '/assets',
        platform: 'browser',
        incremental: trueBoolean,
        color: trueBoolean,
        logOverride: {
            // Happens in Emscripten-generated MSDF (msdfgen_wasm.js):
            //    1 │ ...typeof module!=="undefined"){module["exports"]=Module}process["o...
            'commonjs-variable-in-esm': 'silent',
            // Happens in Emscripten-generated MSDF (msdfgen_wasm.js):
            //    1 │ ...y{table.grow(1)}catch(err){if(!err instanceof RangeError){throw ...
            'suspicious-boolean-not': 'silent',
        },
        /* eslint-enable @typescript-eslint/naming-convention */
    } satisfies esbuild.BuildOptions
    // The narrower type is required to avoid non-null assertions elsewhere.
    // The intersection with `esbuild.BuildOptions` is required to allow mutation.
    const correctlyTypedBuildOptions: esbuild.BuildOptions & typeof buildOptions = buildOptions
    return correctlyTypedBuildOptions
}

/** The basic, common settings for the bundler, based on the environment variables.
 *
 * Note that they should be further customized as per the needs of the specific workflow (e.g. watch vs. build).
 */
export function bundlerOptionsFromEnv() {
    return bundlerOptions(argumentsFromEnv())
}

/** ESBuild options for spawning a watcher, that will continuously rebuild the package. */
export function watchOptions(onRebuild?: () => void, inject?: esbuild.BuildOptions['inject']) {
    return esbuildWatch.toWatchOptions(bundlerOptionsFromEnv(), onRebuild, inject)
}

/** ESBuild options for bundling (one-off build) the package.
 *
 * Relies on the environment variables to be set.
 */
export function bundleOptions() {
    const ret = bundlerOptionsFromEnv()
    ret.watch = false
    ret.incremental = false
    return ret
}

/**
 * Bundles the package.
 */
export async function bundle() {
    try {
        return esbuild.build(bundleOptions())
    } catch (error) {
        console.error(error)
        throw error
    }
}

export default { watchOptions, bundle, bundleOptions }
