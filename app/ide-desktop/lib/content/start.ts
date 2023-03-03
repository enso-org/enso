import bundler from './esbuild-config.js'
// @ts-ignore
import * as server from 'enso-gui-server'
import esbuild from 'esbuild'

const opts = bundler.bundleOptions()
const root = opts.outdir
const assets = root
await esbuild.build(opts)
await server.start({ root, assets })
