import path from 'node:path'
import url from 'node:url'
import portfinder from 'portfinder'
import connect from 'connect'
import { WebSocketServer } from 'ws'
import serveStatic from 'serve-static'
import logger from 'morgan'

export const DEFAULT_PORT = 8080

let dirname = path.dirname(url.fileURLToPath(import.meta.url))
// Path of a file that needs to be injected into the bundle for live-reload to work.
export const LIVE_RELOAD_LISTENER_PATH = path.join(dirname, 'live-reload.js')

export async function start({ root, assets, port }) {
    assets = assets ?? path.join(root, 'assets')

    const freePort = await portfinder.getPortPromise({ port: port ?? DEFAULT_PORT })

    // Added to test if it fixes the issue: https://github.com/expressjs/serve-static/issues/155
    // It does not, should probably be removed. To be checked when the issue is resolved.
    const setHeaders = (res) => {
        res.setHeader('Cache-Control', 'no-store')
    }

    const app = connect()
        .use(logger('dev', { skip: (req, res) => res.statusCode < 400 }))
        .use(serveStatic(root, {setHeaders}))
        .use('/assets', serveStatic(assets, {setHeaders}))

    const server = app.listen(freePort)
    const wsServer = new WebSocketServer({ server, clientTracking: true, path: '/live-reload' })

    var serverUrl = `http://localhost:${freePort}`
    console.log('Serving %s', serverUrl)

    return {
        port: freePort,
        reload() {
            wsServer.clients.forEach(sock => sock.send('reload'))
        },
    }
}
