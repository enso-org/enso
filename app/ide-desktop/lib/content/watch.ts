import bundler from './esbuild-config.js'
// @ts-ignore
import * as server from 'enso-gui-server'

const reloadListenerCode =
    '(() => window.live_reload = window.live_reload ?? (new EventSource("/live-reload").onmessage = () => location.reload()))();'
await bundler.watch(() => live_server?.reload(), { js: reloadListenerCode })

const live_server = await server.start({
    root: bundler.output_path,
    assets: bundler.output_path,
})
