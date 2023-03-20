/** @file Live-reload server implementation. */

import * as path from 'node:path'
import * as url from 'node:url'

import * as portfinder from 'portfinder'
import * as ws from 'ws'
import connect from 'connect'
import logger from 'morgan'
import serveStatic from 'serve-static'

// =================
// === Constants ===
// =================

export const DEFAULT_PORT = 8080
export const HTTP_STATUS_BAD_REQUEST = 400

const DIR_NAME = path.dirname(url.fileURLToPath(import.meta.url))
/** Path of a file that needs to be injected into the bundle for live-reload to work. */
export const LIVE_RELOAD_LISTENER_PATH = path.join(DIR_NAME, 'live-reload.js')

/** Start the server.
 *
 * @param {{ root: string; assets?: string | null; port?: number; }} options - Configuration options for this server.
 */
export async function start({ root, assets, port }) {
    assets = assets ?? path.join(root, 'assets')

    const freePort = await portfinder.getPortPromise({ port: port ?? DEFAULT_PORT })

    // FIXME: There is an issue probably related with improper caches of served files. Read more
    //     here: https://github.com/expressjs/serve-static/issues/155
    const app = connect()
        .use(logger('dev', { skip: (_req, res) => res.statusCode < HTTP_STATUS_BAD_REQUEST }))
        .use(serveStatic(root))
        .use('/assets', serveStatic(assets))

    const server = app.listen(freePort)
    const wsServer = new ws.WebSocketServer({ server, clientTracking: true, path: '/live-reload' })

    var serverUrl = `http://localhost:${freePort}`
    console.log('Serving %s', serverUrl)

    return {
        port: freePort,
        reload() {
            wsServer.clients.forEach(sock => {
                sock.send('reload')
            })
        },
    }
}
