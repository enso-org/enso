/** @file Esbuild config file. */
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'

import type * as esbuild from 'esbuild'
import { wasmLoader } from 'esbuild-plugin-wasm'

import * as appConfig from 'enso-common/src/appConfig'
import * as paths from './paths'

// ====================
// === Global setup ===
// ====================

await appConfig.readEnvironmentFromFile()

// ================
// === Bundling ===
// ================

/**
 * Get the bundler options using the environment.
 *
 * The following environment variables are required:
 * - `ENSO_BUILD_IDE` - output directory for bundled client files;
 * - `ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH` - path to the project manager executable relative
 * to the PM bundle root;
 * @see bundlerOptions
 */
export function bundlerOptionsFromEnv(devMode = false): esbuild.BuildOptions {
  return bundlerOptions(
    path.join(paths.getIdeDirectory(), 'client'),
    paths.getProjectManagerInBundlePath(),
    devMode,
  )
}

/** Get options without relying on the environment. */
export function bundlerOptions(
  outdir: string,
  projectManagerInBundlePath: string,
  devMode = false,
): esbuild.BuildOptions {
  return {
    bundle: true,
    outdir,
    entryPoints: ['src/index.ts', 'src/preload.ts'],
    outbase: 'src',
    format: 'esm',
    platform: 'node',
    outExtension: { '.js': '.mjs' },
    plugins: [wasmLoader()],
    target: ['node20'], // electron31
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
    define: {
      'process.env.PROJECT_MANAGER_IN_BUNDLE_PATH': JSON.stringify(projectManagerInBundlePath),
      'process.env.ELECTRON_DEV_MODE': JSON.stringify(String(devMode)),
      'process.env.GUI_CONFIG_PATH': JSON.stringify(path.resolve('../../gui/vite.config.ts')),
    },
    sourcemap: 'linked',
    external: ['electron', 'vite', 'lightningcss'],
  }
}
