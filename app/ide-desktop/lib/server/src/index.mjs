import path from 'node:path'
import liveServer from 'live-server'
import * as portfinder from 'portfinder'

export const DEFAULT_PORT = 8080

async function findPort(startPort = DEFAULT_PORT) {
    return portfinder.getPortPromise({ startPort, port: startPort })
}

export async function start({ root, assets, port }) {
    assets = assets ?? path.join(root, 'assets')
    const parameters = {
        cors: true,
        open: false, // When false, it won't load your browser by default.
        // When set, serve this file (server root relative) for every 404 (useful for single-page
        // applications).
        file: '/assets/index.html',
        wait: 0, // Waits for all changes, before reloading. Defaults to 0 sec.
        logLevel: 2, // 0 = errors only, 1 = some, 2 = lots
        port: await findPort(port ?? DEFAULT_PORT),
        root: root ?? '.',
        assets,
        mount: [['/assets', assets]], // Mount a directory to a route.
    }
    console.log(`Server configuration:`, parameters)
    const server = liveServer.start(parameters)
    console.log(`Server started.`)
    return parameters.port
}
