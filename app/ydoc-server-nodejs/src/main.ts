/** @file entrypoint for standalone ydoc-server running in nodejs environment. */

import './cjs-shim' // must be imported first

import * as http from 'node:http'
import { createGatewayServer } from 'ydoc-server'

const DEFAULT_PORT = 5976
const PORT = (process.env.PORT != null && parseInt(process.env.PORT, 10)) || DEFAULT_PORT
const HOSTNAME = process.env.GUI_HOSTNAME ?? 'localhost'
const LANGUAGE_SERVER_URL = process.env.LANGUAGE_SERVER_URL

await runServer()

/** Start http server that handles ydoc websocket connections. */
async function runServer() {
  const server = http.createServer()
  server.on('request', (request, response) => {
    if (request.method === 'GET' && request.url === '/_health') {
      response.writeHead(200, { 'Content-Type': 'text/plain; charset=UTF-8' }).end('OK')
    }
  })

  await createGatewayServer(server, LANGUAGE_SERVER_URL)

  server.listen(PORT, HOSTNAME, () => {
    console.log(`Ydoc server listening on ${HOSTNAME}:${PORT}`)
  })
}
