/** @file Configuration for the esbuild bundler and build/watch commands.
 *
 * The bundler processes each entry point into a single file, each with no external dependencies and
 * minified. This primarily involves resolving all imports, along with some other transformations
 * (like TypeScript compilation).
 *
 * See the bundlers documentation for more information:
 * https://esbuild.github.io/getting-started/#bundling-for-node. */
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import esbuildPluginTime from 'esbuild-plugin-time'

// =================
// === Constants ===
// =================

const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// =============================
// === Environment variables ===
// =============================

/** Mandatory build options. */
export interface Arguments {
    /** Path where bundled files are output. */
    outputPath: string
    /** `true` if in development mode (live-reload), `false` if in production mode. */
    devMode: boolean
}

// ================
// === Bundling ===
// ================

/** Generate the bundler options. */
export function bundlerOptions(args: Arguments) {
    const { outputPath, devMode } = args
    // This is required to prevent TypeScript from narrowing `true` to `boolean`.
    // eslint-disable-next-line no-restricted-syntax
    const trueBoolean = true as boolean
    const buildOptions = {
        absWorkingDir: THIS_PATH,
        bundle: trueBoolean,
        entryPoints: [path.resolve(THIS_PATH, 'src', 'index.ts')],
        outfile: path.join(outputPath, 'index.cjs'),
        outbase: 'src',
        plugins: [esbuildPluginTime()],
        pure: ['assert'],
        sourcemap: trueBoolean,
        minify: !devMode,
        metafile: trueBoolean,
        format: 'cjs',
        platform: 'node',
        color: trueBoolean,
    } satisfies esbuild.BuildOptions
    // The narrower type is required to avoid non-null assertions elsewhere.
    // The intersection with `esbuild.BuildOptions` is required to allow adding extra properties.
    const correctlyTypedBuildOptions: esbuild.BuildOptions & typeof buildOptions = buildOptions
    return correctlyTypedBuildOptions
}
