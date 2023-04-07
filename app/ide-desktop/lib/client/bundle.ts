/** @file Script that bundles JS client code. */

import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'
import * as dashboardBundler from '../dashboard/esbuild-config'

// =================
// === Constants ===
// =================

export const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// ================
// === Bundling ===
// ================

// The dashboard bundler bundles `tailwind.css`.
const DASHBOARD_BUNDLER_OPTIONS = dashboardBundler.bundleOptions()
await esbuild.build(DASHBOARD_BUNDLER_OPTIONS)
const BUNDLER_OPTIONS = bundler.bundlerOptionsFromEnv()
await esbuild.build(BUNDLER_OPTIONS)
