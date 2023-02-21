/** @file File watch and compile service. */
import bundler from './esbuild-config.js'
import * as server from 'enso-gui-server'

await bundler.watch(() => liveServer.reload(), [server.LIVE_RELOAD_LISTENER_PATH])

const liveServer = await server.start({
    root: bundler.output_path,
    assets: bundler.output_path,
})
