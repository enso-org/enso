/** @file Entry point for the bundler. */
import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

try {
    void esbuild.build(bundler.bundleOptions())
} catch (error) {
    console.error(error)
    throw error
}
