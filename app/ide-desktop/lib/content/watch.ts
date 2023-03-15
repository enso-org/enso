/** @file File watch and compile service. */
import * as esbuild from 'esbuild'

import * as guiServer from 'enso-gui-server'

import bundler from './esbuild-config.js'

const OPTS = bundler.watchOptions(() => {
    LIVE_SERVER.reload()
}, [guiServer.LIVE_RELOAD_LISTENER_PATH])
await esbuild.build(OPTS)
const LIVE_SERVER = await guiServer.start({
    root: OPTS.outdir,
    assets: OPTS.outdir,
})
