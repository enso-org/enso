import './cjs-shim' // must be imported first

import http from 'http'
import { createGatewayServer } from 'ydoc-server'

const port = process.env.PORT ? +process.env.PORT : 5976
const hostname = process.env.HOSTNAME ?? 'localhost'
const overrideLanguageServerUrl = process.env.LANGUAGE_SERVER_URL

const server = http.createServer()
await createGatewayServer(server, overrideLanguageServerUrl)
server.listen(port, hostname, () => {
  console.log(`Ydoc server listening on ${hostname}:${port}`)
})
