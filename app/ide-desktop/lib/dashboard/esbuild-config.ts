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
import * as os from 'node:os'
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import * as esbuildPluginNodeModules from '@esbuild-plugins/node-modules-polyfill'
import esbuildPluginTime from 'esbuild-plugin-time'

// =================
// === Constants ===
// =================

const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// =============================
// === Environment variables ===
// =============================

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
    const { outputPath } = args
    const buildOptions = {
        absWorkingDir: THIS_PATH,
        bundle: true,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        loader: { '.html': 'copy' },
        entryPoints: [
            path.resolve(THIS_PATH, 'src', 'index.tsx'),
            path.resolve(THIS_PATH, 'src', 'serviceWorker.ts'),
        ],
        outdir: outputPath,
        outbase: 'src',
        plugins: [
            esbuildPluginNodeModules.NodeModulesPolyfillPlugin(),
            esbuildPluginTime(),
        ],
        define: {
            // We are defining a constant, so it should be `CONSTANT_CASE`.
            // eslint-disable-next-line @typescript-eslint/naming-convention
            IS_DEV_MODE: JSON.stringify(args.devMode),
        },
        sourcemap: true,
        minify: true,
        metafile: true,
        format: 'esm',
        platform: 'browser',
        color: true,
    } satisfies esbuild.BuildOptions
    // The narrower type is required to avoid non-null assertions elsewhere.
    // The intersection with `esbuild.BuildOptions` is required to allow mutation.
    const correctlyTypedBuildOptions: esbuild.BuildOptions & typeof buildOptions = buildOptions
    return correctlyTypedBuildOptions
}

/** Common settings for the bundler.
 *
 * Note that they should be further customized as per the needs of the specific workflow
 * (e.g. watch vs. build). */
export function defaultBundlerOptions(args: Pick<Arguments, 'devMode'>) {
    // FIXME[sb]: This `outputPath` is extremely hacky.
    return bundlerOptions({ outputPath: path.join(os.tmpdir(), 'enso-dashboard'), devMode: args.devMode })
}

/** ESBuild options for bundling (one-off build) the package.
 *
 * Relies on the environment variables to be set. */
export function bundleOptions() {
    return defaultBundlerOptions({ devMode: false })
}
