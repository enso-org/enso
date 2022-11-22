import bundler from './esbuild-config.js'
// @ts-ignore
import * as server from 'enso-gui-server'

await bundler.watch(() => liveServer?.reload(), [server.LIVE_RELOAD_LISTENER_PATH])

const liveServer = await server.start({
    root: bundler.output_path,
    assets: bundler.output_path,
})
