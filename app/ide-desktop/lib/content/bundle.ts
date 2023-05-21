/** @file Entry point for the bundler. */
import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

// =======================
// === Generate bundle ===
// =======================

try {
    void esbuild.build(
        bundler.bundleOptions({
            devMode: false,
            supportsLocalBackend: true,
            supportsDeepLinks: true,
        })
    )
} catch (error) {
    console.error(error)
    throw error
}
