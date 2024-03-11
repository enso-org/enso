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
 * - `ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION` - version of the Engine (backend) that is bundled
 * along with this client build.
 * @see bundlerOptions
 */
export function bundlerOptionsFromEnv(devMode = false): esbuild.BuildOptions {
    return bundlerOptions(
        path.join(paths.getIdeDirectory(), 'client'),
        paths.getProjectManagerInBundlePath(),
        paths.getBundledEngineVersion(),
        devMode
    )
}

/** Get options without relying on the environment. */
export function bundlerOptions(
    outdir: string,
    projectManagerInBundlePath: string,
    bundledEngineVersion: string,
    devMode = false
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
            BUNDLED_ENGINE_VERSION: JSON.stringify(bundledEngineVersion),
            PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(projectManagerInBundlePath),
            'process.env.DEV_MODE': JSON.stringify(String(devMode)),
            'process.env.GUI2_CONFIG_PATH': JSON.stringify(
                path.resolve('../../../gui2/vite.config.ts')
            ),
        },
        /* eslint-enable @typescript-eslint/naming-convention */
        sourcemap: true,
        external: ['electron', 'vite', 'lightningcss'],
    }
}
