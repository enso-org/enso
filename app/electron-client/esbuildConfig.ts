/** @file Esbuild config file. */
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'

import type * as esbuild from 'esbuild'
import { wasmLoader } from 'esbuild-plugin-wasm'

import * as paths from './paths'

/**
 * Get the bundler options using the environment.
 *
 * The following environment variables are required:
 * - `ENSO_BUILD_IDE` - output directory for bundled client files;
 * @see bundlerOptions
 */
export function bundlerOptionsFromEnv(devMode = false): esbuild.BuildOptions {
  return bundlerOptions(path.join(paths.getIdeDirectory(), 'client'), devMode)
}

/** Get options without relying on the environment. */
export function bundlerOptions(outdir: string, devMode = false): esbuild.BuildOptions {
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
      'process.env.ELECTRON_DEV_MODE': JSON.stringify(String(devMode)),
      'process.env.ENSO_IDE_VERSION': JSON.stringify(process.env.ENSO_IDE_VERSION),
      'process.env.ENSO_IDE_COMMIT_HASH': JSON.stringify(process.env.ENSO_IDE_COMMIT_HASH),
    },
    sourcemap: 'linked',
    external: ['electron', 'vite', 'lightningcss', 'original-fs'],
  }
}
