/** @file Script that bundles JS client code. */
import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

// ================
// === Bundling ===
// ================

await esbuild.build(bundler.bundlerOptionsFromEnv())
