/** @file File watch and compile service. */
import bundler from './esbuild-config.js'
import * as server from 'enso-gui-server'
import esbuild from 'esbuild'

const opts = bundler.watchOptions(() => liveServer.reload(), [server.LIVE_RELOAD_LISTENER_PATH])
await esbuild.build(opts)
const liveServer = await server.start({
    root: opts.outdir,
    assets: opts.outdir,
})
