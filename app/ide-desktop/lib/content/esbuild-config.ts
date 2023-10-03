/** @file Configuration for the esbuild bundler and build/watch commands.
 *
 * The bundler processes each entry point into a single file, each with no external dependencies and
 * minified. This primarily involves resolving all imports, along with some other transformations
 * (like TypeScript compilation).
 *
 * See the bundlers documentation for more information:
 * https://esbuild.github.io/getting-started/#bundling-for-node. */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'
import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import * as esbuildPluginNodeGlobals from '@esbuild-plugins/node-globals-polyfill'
import * as esbuildPluginNodeModules from '@esbuild-plugins/node-modules-polyfill'
import esbuildPluginCopyDirectories from 'esbuild-plugin-copy-directories'
import esbuildPluginTime from 'esbuild-plugin-time'
import esbuildPluginYaml from 'esbuild-plugin-yaml'

import * as utils from '../../utils'
import BUILD_INFO from '../../../../build.json' assert { type: 'json' }

// =================
// === Constants ===
// =================

const THIS_PATH = pathModule.resolve(pathModule.dirname(url.fileURLToPath(import.meta.url)))

// =============================
// === Environment variables ===
// =============================

/** Arguments that must always be supplied, because they are not defined as
 * environment variables. */
export interface PassthroughArguments {
    /** `true` if in development mode (live-reload), `false` if in production mode. */
    devMode: boolean
    /** Whether the application may have the local backend running. */
    supportsLocalBackend: boolean
    /** Whether the application supports deep links. This is only true when using
     * the installed app on macOS and Windows. */
    supportsDeepLinks: boolean
}

/** Mandatory build options. */
export interface Arguments extends PassthroughArguments {
    /** List of files to be copied from WASM artifacts. */
    wasmArtifacts: string
    /** Directory with assets. Its contents are to be copied. */
    assetsPath: string
    /** Path where bundled files are output. */
    outputPath: string
}

/** Get arguments from the environment. */
export function argumentsFromEnv(passthroughArguments: PassthroughArguments): Arguments {
    const wasmArtifacts = utils.requireEnv('ENSO_BUILD_GUI_WASM_ARTIFACTS')
    const assetsPath = utils.requireEnv('ENSO_BUILD_GUI_ASSETS')
    const outputPath = pathModule.resolve(utils.requireEnv('ENSO_BUILD_GUI'), 'assets')
    return { ...passthroughArguments, wasmArtifacts, assetsPath, outputPath }
}

// ===================
// === Git process ===
// ===================

/** Get output of a git command.
 * @param command - Command line following the `git` program.
 * @returns Output of the command. */
function git(command: string): string {
    // TODO [mwu] Eventually this should be removed, data should be provided by the build script
    //            through `BUILD_INFO`. The bundler configuration should not invoke git,
    //            it is not its responsibility.
    return childProcess.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
}

// ================
// === Bundling ===
// ================

/** Generate the builder options. */
export function bundlerOptions(args: Arguments) {
    const {
        outputPath,
        wasmArtifacts,
        assetsPath,
        devMode,
        supportsLocalBackend,
        supportsDeepLinks,
    } = args
    const buildOptions = {
        // The names come from a third-party API and cannot be changed.
        /* eslint-disable @typescript-eslint/naming-convention */
        absWorkingDir: THIS_PATH,
        bundle: true,
        loader: {
            '.html': 'copy',
            '.css': 'copy',
            '.map': 'copy',
            '.wasm': 'copy',
            '.svg': 'dataurl',
            '.png': 'file',
            '.ttf': 'copy',
        },
        entryPoints: [
            pathModule.resolve(THIS_PATH, 'src', 'index.ts'),
            pathModule.resolve(THIS_PATH, 'src', 'index.html'),
            pathModule.resolve(THIS_PATH, 'src', 'run.js'),
            pathModule.resolve(THIS_PATH, 'src', 'style.css'),
            pathModule.resolve(THIS_PATH, 'src', 'serviceWorker.ts'),
            ...wasmArtifacts.split(pathModule.delimiter),
            ...fsSync
                .readdirSync(assetsPath)
                .map(fileName => pathModule.resolve(assetsPath, fileName)),
        ].map(path => ({ in: path, out: pathModule.basename(path, pathModule.extname(path)) })),
        outdir: outputPath,
        outbase: 'src',
        plugins: [
            {
                name: 'override-loaders',
                setup: build => {
                    // This file MUST be in CommonJS format because it is loaded using `Function()`
                    // in `ensogl/pack/js/src/runner/index.ts`.
                    // All other files are ESM because of `"type": "module"` in `package.json`.
                    build.onLoad({ filter: /[/\\]pkg\.js$/ }, async info => {
                        const { path } = info
                        return {
                            contents: await fs.readFile(path),
                            loader: 'copy',
                        }
                    })
                    // `.png` and `.svg` files not in the `assets` module should not use the `file`
                    // loader.
                    build.onLoad({ filter: /(?:\.png|\.svg)$/ }, async info => {
                        const { path } = info
                        if (!/[/\\]assets[/\\][^/\\]*(?:\.png|\.svg)$/.test(path)) {
                            return {
                                contents: await fs.readFile(path),
                                loader: 'copy',
                            }
                        } else {
                            return
                        }
                    })
                },
            },
            esbuildPluginCopyDirectories(),
            esbuildPluginYaml.yamlPlugin({}),
            esbuildPluginNodeModules.NodeModulesPolyfillPlugin(),
            esbuildPluginNodeGlobals.NodeGlobalsPolyfillPlugin({ buffer: true, process: true }),
            esbuildPluginTime(),
        ],
        define: {
            GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
            GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
            BUILD_INFO: JSON.stringify(BUILD_INFO),
            /** Whether the application is being run locally. This enables a service worker that
             * properly serves `/index.html` to client-side routes like `/login`. */
            IS_DEV_MODE: JSON.stringify(devMode),
            /** Overrides the redirect URL for OAuth logins in the production environment.
             * This is needed for logins to work correctly under `./run gui watch`. */
            REDIRECT_OVERRIDE: 'undefined',
            CLOUD_ENV:
                process.env.ENSO_CLOUD_ENV != null
                    ? JSON.stringify(process.env.ENSO_CLOUD_ENV)
                    : 'undefined',
            SUPPORTS_LOCAL_BACKEND: JSON.stringify(supportsLocalBackend),
            SUPPORTS_DEEP_LINKS: JSON.stringify(supportsDeepLinks),
        },
        pure: ['assert'],
        sourcemap: true,
        minify: !devMode,
        metafile: true,
        format: 'esm',
        platform: 'browser',
        color: true,
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
 * Note that they should be further customized as per the needs of the specific workflow
 * (e.g. watch vs. build). */
export function bundlerOptionsFromEnv(passthroughArguments: PassthroughArguments) {
    return bundlerOptions(argumentsFromEnv(passthroughArguments))
}

/** esbuild options for bundling the package for a one-off build.
 *
 * Relies on the environment variables to be set. */
export function bundleOptions(passthroughArguments: PassthroughArguments) {
    return bundlerOptionsFromEnv(passthroughArguments)
}
