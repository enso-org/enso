import path from 'node:path'
import esbuild from 'esbuild'
import { require_env, require_env_resolved_path } from '../../utils.js'
import { getBundledEngineVersion, getIdeDirectory, getProjectManagerInBundlePath } from './paths.js'

// ===================================================
// === Constants provided through the environment. ===
// ===================================================

/** Output directory for bundled client files. */
export const outdir = path.join(require_env_resolved_path('ENSO_BUILD_IDE'), 'client')

/** Path to the project manager executable relative to the PM bundle root. */
export const projectManagerInBundlePath = require_env('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')

/** Version of the Engine (backend) that is bundled along with this client build. */
export const bundledEngineVersion = require_env('ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION')

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
 **/
export function bundlerOptionsFromEnv(): esbuild.BuildOptions {
    return bundlerOptions(
        path.join(getIdeDirectory(), 'client'),
        getProjectManagerInBundlePath(),
        getBundledEngineVersion()
    )
}

/// Get options without relying on the environment
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
        outExtension: { '.js': '.cjs' },
        platform: 'node',
        define: {
            BUNDLED_ENGINE_VERSION: JSON.stringify(bundledEngineVersion),
            PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(projectManagerInBundlePath),
        },
        sourcemap: true,
        external: ['electron'],
    }
}
