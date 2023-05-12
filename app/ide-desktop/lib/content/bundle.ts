/** @file Entry point for the bundler. */
import * as esbuild from 'esbuild'

import * as common from 'enso-common'

import * as bundler from './esbuild-config'

try {
    void esbuild.build(
        bundler.bundleOptions({
            devMode: false,
            platform: common.Platform.desktop,
        })
    )
} catch (error) {
    console.error(error)
    throw error
}
