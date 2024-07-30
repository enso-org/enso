import './cjs-shim' // must be imported first

import http from 'http'
import { createGatewayServer } from 'ydoc-server'

const server = http.createServer()
await createGatewayServer(server)

const port = 1234
const hostname = '127.0.0.1'
server.listen(port, hostname, () => {
  console.log(`Ydoc server listening on ${hostname}:${port}`)
})
