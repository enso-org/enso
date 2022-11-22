import path from 'node:path'
import url from 'node:url'
import portfinder from 'portfinder'
import connect from 'connect'
// import { WebSocket } from 'faye-websocket'
import { WebSocketServer } from 'ws'
import serveStatic from 'serve-static'
import logger from 'morgan'

export const DEFAULT_PORT = 8080

let dirname = path.dirname(url.fileURLToPath(import.meta.url))
// Path of a file that needs to be injected into the bundle for live-reload to work.
export const LIVE_RELOAD_LISTENER_PATH = path.join(dirname, 'live-reload.js')

export async function start({ root, assets, port }) {
    assets = assets ?? path.join(root, 'assets')

    const freePort = await portfinder.getPortPromise({ port: port ?? DEFAULT_PORT});

    const app = connect()
        .use(logger('dev', { skip: (req, res) => res.statusCode < 400 }))
        .use(serveStatic(root))
        .use('/assets', serveStatic(assets))

    const server = app.listen(freePort);

    const reloadSockets = []
    const wsServer = new WebSocketServer({ server, path: '/live-reload' });
    wsServer.on('connection', (socket) => reloadSockets.push(socket));

    var serverUrl = `http://localhost:${freePort}`;
    console.log(("Serving %s"), serverUrl);

    return {
        port: freePort,
        reload() {
            reloadSockets.forEach((sock) => sock.send("reload"));
            reloadSockets.length = 0;
        }
    }
}
