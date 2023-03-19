/** @file Start the file watch service. */
import * as esbuild from 'esbuild'

import * as guiServer from 'enso-gui-server'

import bundler from './esbuild-config.js'

const OPTS = bundler.bundleOptions()
const ROOT = OPTS.outdir
const ASSETS = ROOT
await esbuild.build(OPTS)
await guiServer.start({ root: ROOT, assets: ASSETS })
