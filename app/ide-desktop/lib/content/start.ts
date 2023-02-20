/** @file start service */
import bundler from './esbuild-config.js'
import * as server from 'enso-gui-server'

await bundler.bundle()
const root = bundler.output_path
const assets = root
await server.start({ root, assets })
