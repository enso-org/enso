/** @file Entry point for the bundler. */
import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

// =======================
// === Generate bundle ===
// =======================

try {
    const options = bundler.bundleOptions({ supportsLocalBackend: true, supportsDeepLinks: true })
    void esbuild.build(options)
} catch (error) {
    console.error(error)
    throw error
}
