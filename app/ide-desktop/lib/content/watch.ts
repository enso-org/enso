import bundler from './esbuild-config.js'
// @ts-ignore
import * as server from 'enso-gui-server'

await bundler.watch()
await server.start({ root: bundler.output_path, assets: bundler.output_path })
