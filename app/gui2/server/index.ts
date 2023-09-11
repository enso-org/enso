import { WebSocketServer, WebSocket } from 'ws'
import { setupWSConnection } from './ydoc'
import { IncomingMessage } from 'node:http'
import Jayson from 'jayson'

import { Server } from 'http'
import { parse } from 'url'

type AuthData = {
  user: string
}

export function createGatewayServer(httpServer: Server) {
  const wss = new WebSocketServer({ noServer: true })
  wss.on('connection', (ws: WebSocket, request: IncomingMessage, _authData: AuthData) => {
    ws.on('error', onWebSocketError)

    const doc = docName(request.url)
    if (doc != null) {
      setupWSConnection(ws, doc)
    }
  })

  httpServer.on('upgrade', (request, socket, head) => {
    console.log('request', request.url)
    if (docName(request.url) != null) {
      socket.on('error', onHttpSocketError)
      authenticate(request, function next(err, user) {
        if (err != null || user == null) {
          socket.write('HTTP/1.1 401 Unauthorized\r\n\r\n')
          socket.destroy()
          return
        }
        socket.removeListener('error', onHttpSocketError)

        wss.handleUpgrade(request, socket, head, function done(ws) {
          wss.emit('connection', ws, request, user)
        })
      })
    }
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
  callback: (err: Error | null, authData: AuthData | null) => void,
) {
  // FIXME: Stub. We don't implement authentication for now. Need to be implemented in combination
  // with the language server.
  const _ = request
  callback(null, { user: 'mock-user' })
}

const docNameRegex = /^[a-z0-9/-]+$/i
function docName(url: string | undefined) {
  if (url == null) return null
  const { pathname } = parse(url)
  const prefix = '/project/'
  if (pathname != null && pathname.startsWith(prefix)) {
    const docName = pathname.slice(prefix.length)
    if (docNameRegex.test(docName)) {
      return docName
    }
  }
  return null
}
