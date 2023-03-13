/** Script that bundles JS client code. */

import path, { dirname } from 'node:path'
import esbuild from 'esbuild'
import { bundlerOptionsFromEnv } from './esbuild-config.js'
import { fileURLToPath } from 'node:url'
export const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))

// ================
// === Bundling ===
// ================

const bundlerOptions: esbuild.BuildOptions = bundlerOptionsFromEnv()
await esbuild.build(bundlerOptions)
