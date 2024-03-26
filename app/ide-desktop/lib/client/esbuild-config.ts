/** @file Esbuild config file. */
import * as path from 'node:path'

import type * as esbuild from 'esbuild'
import esbuildPluginYaml from 'esbuild-plugin-yaml'

import * as appConfig from 'enso-common/src/appConfig'
import * as paths from './paths'

// ====================
// === Global setup ===
// ====================

await appConfig.readEnvironmentFromFile()

// ================
// === Bundling ===
// ================

/** Get the bundler options using the environment.
 *
 * The following environment variables are required:
 * - `ENSO_BUILD_IDE` - output directory for bundled client files;
 * - `ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH` - path to the project manager executable relative
 * to the PM bundle root;
 * @see bundlerOptions
 */
export function bundlerOptionsFromEnv(): esbuild.BuildOptions {
    return bundlerOptions(
        path.join(paths.getIdeDirectory(), 'client'),
        paths.getProjectManagerInBundlePath()
    )
}

/** Get options without relying on the environment. */
export function bundlerOptions(
    outdir: string,
    projectManagerInBundlePath: string
): esbuild.BuildOptions {
    return {
        bundle: true,
        outdir,
        entryPoints: ['src/index.ts', 'src/preload.ts'],
        outbase: 'src',
        format: 'cjs',
        platform: 'node',
        plugins: [esbuildPluginYaml.yamlPlugin({})],
        // The names come from a third-party API and cannot be changed.
        /* eslint-disable @typescript-eslint/naming-convention */
        outExtension: { '.js': '.cjs' },
        define: {
            PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(projectManagerInBundlePath),
        },
        /* eslint-enable @typescript-eslint/naming-convention */
        sourcemap: true,
        external: ['electron'],
    }
}
