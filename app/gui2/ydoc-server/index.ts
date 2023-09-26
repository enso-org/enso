/**
 * @file An entry point for the Yjs gateway server. The gateway server is a WebSocket server that
 * synchronizes document requests and updates between language server and clients connected to the
 * Yjs document mesh. It also serves as a central point for synchronizing document data and
 * awareness updates between clients.
 *
 * Currently, this server is being run automatically in background as part of the vite development
 * server. It is not yet deployed to any other environment.
 */

import { Server } from 'http'
import { IncomingMessage } from 'node:http'
import { parse } from 'url'
import { WebSocket, WebSocketServer } from 'ws'
import { setupGatewayClient } from './ydoc'

type ConnectionData = {
  lsUrl: string
  doc: string
  user: string
}

export function createGatewayServer(httpServer: Server) {
  const wss = new WebSocketServer({ noServer: true })
  wss.on('connection', (ws: WebSocket, request: IncomingMessage, data: ConnectionData) => {
    ws.on('error', onWebSocketError)
    setupGatewayClient(ws, data.lsUrl, data.doc)
  })

  httpServer.on('upgrade', (request, socket, head) => {
    socket.on('error', onHttpSocketError)
    authenticate(request, function next(err, data) {
      if (err != null) {
        socket.write('HTTP/1.1 401 Unauthorized\r\n\r\n')
        socket.destroy()
        return
      }
      socket.removeListener('error', onHttpSocketError)
      if (data != null) {
        wss.handleUpgrade(request, socket, head, function done(ws) {
          wss.emit('connection', ws, request, data)
        })
      }
    })
  })
}

function onWebSocketError(err: Error) {
  console.log('WebSocket error:', err)
}

function onHttpSocketError(err: Error) {
  console.log('HTTP socket error:', err)
}

function authenticate(
  request: IncomingMessage,
  callback: (err: Error | null, authData: ConnectionData | null) => void,
) {
  // FIXME: Stub. We don't implement authentication for now. Need to be implemented in combination
  // with the language server.
  const user = 'mock-user'

  if (request.url == null) return callback(null, null)
  const { pathname, query } = parse(request.url, true)
  if (pathname == null) return callback(null, null)
  const doc = docName(pathname)
  const lsUrl = query.ls
  const data = doc != null && typeof lsUrl === 'string' ? { lsUrl, doc, user } : null
  callback(null, data)
}

const docNameRegex = /^[a-z0-9/-]+$/i
function docName(pathname: string) {
  const prefix = '/project/'
  if (pathname != null && pathname.startsWith(prefix)) {
    const docName = pathname.slice(prefix.length)
    if (docNameRegex.test(docName)) {
      return docName
    }
  }
  return null
}
