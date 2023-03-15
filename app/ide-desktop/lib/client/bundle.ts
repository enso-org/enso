/** @file Script that bundles JS client code. */

import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'

import * as esbuildConfig from './esbuild-config.js'

// =================
// === Constants ===
// =================

export const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// ================
// === Bundling ===
// ================

const BUNDLER_OPTIONS: esbuild.BuildOptions = esbuildConfig.bundlerOptionsFromEnv()
await esbuild.build(BUNDLER_OPTIONS)
