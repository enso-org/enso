/**
 * @file An entry point for the Yjs gateway server. The gateway server is a WebSocket server that
 * synchronizes document requests and updates between language server and clients connected to the
 * Yjs document mesh. It also serves as a central point for synchronizing document data and
 * awareness updates between clients.
 *
 * Currently, this server is being run automatically in background as part of the vite development
 * server. It is not yet deployed to any other environment.
 */

import debug from 'debug'
import type { Server } from 'http'
import type { Http2SecureServer } from 'http2'
import type { WebSocket } from 'isomorphic-ws'
import type { IncomingMessage } from 'node:http'
import { ConnectionData, docName } from './auth'
import { deserializeIdMap } from './serialization'
import { setupGatewayClient } from './ydoc'

export { deserializeIdMap, docName, setupGatewayClient }

/** @param customLogger Optional external logger to use for all debug logs. */
export function configureAllDebugLogs(
  forceEnable: boolean,
  customLogger?: (...args: any[]) => any,
) {
  for (const debugModule of ['ydoc-server:session', 'ydoc-shared:languageServer']) {
    const instance = debug(debugModule)
    if (forceEnable) instance.enabled = true
    if (customLogger) instance.log = customLogger
  }
}

/** Create a WebSocket server to host the YDoc coordinating server. */
export async function createGatewayServer(
  httpServer: Server | Http2SecureServer,
  overrideLanguageServerUrl?: string,
) {
  const { WebSocketServer } = await import('isomorphic-ws')
  const { parse } = await import('node:url')

  const wss = new WebSocketServer({ noServer: true })
  wss.on('connection', (ws: WebSocket, _request: IncomingMessage, data: ConnectionData) => {
    ws.on('error', onWebSocketError)
    setupGatewayClient(ws, overrideLanguageServerUrl ?? data.lsUrl, data.doc)
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
}
