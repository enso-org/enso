/** @file Esbuild config file. */
import * as path from 'node:path'

import * as esbuild from 'esbuild'

import * as paths from './paths.js'
import * as utils from '../../utils.js'

// ===================================================
// === Constants provided through the environment. ===
// ===================================================

/** Output directory for bundled client files. */
export const OUT_DIR_PATH = path.join(utils.requireEnvResolvedPath('ENSO_BUILD_IDE'), 'client')

/** Path to the project manager executable relative to the PM bundle root. */
export const PROJECT_MANAGER_IN_BUNDLE_PATH = utils.requireEnv(
    'ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH'
)

/** Version of the Engine (backend) that is bundled along with this client build. */
export const BUNDLED_ENGINE_VERSION = utils.requireEnv('ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION')

// ================
// === Bundling ===
// ================

/**
 * Get the bundler options using the environment.
 *
 * The following environment variables are required:
 * - `ENSO_BUILD_IDE` - output directory for bundled client files;
 * - `ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH` - path to the project manager executable relative to the PM bundle root;
 * - `ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION` - version of the Engine (backend) that is bundled along with this client build.
 *
 * @see bundlerOptions
 */
export function bundlerOptionsFromEnv(): esbuild.BuildOptions {
    return bundlerOptions(
        path.join(paths.getIdeDirectory(), 'client'),
        paths.getProjectManagerInBundlePath(),
        paths.getBundledEngineVersion()
    )
}

/** Get options without relying on the environment. */
export function bundlerOptions(
    outdir: string,
    projectManagerInBundlePath: string,
    bundledEngineVersion: string
): esbuild.BuildOptions {
    return {
        bundle: true,
        outdir,
        entryPoints: ['src/index.ts', 'src/preload.ts'],
        outbase: 'src',
        format: 'cjs',
        // Disabling naming convnetion lints below
        // because they are third-party configuration options.
        // eslint-disable-next-line @typescript-eslint/naming-convention
        outExtension: { '.js': '.cjs' },
        platform: 'node',
        define: {
            /* eslint-disable @typescript-eslint/naming-convention */
            BUNDLED_ENGINE_VERSION: JSON.stringify(bundledEngineVersion),
            PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(projectManagerInBundlePath),
            /* eslint-enable @typescript-eslint/naming-convention */
        },
        sourcemap: true,
        external: ['electron'],
    }
}
