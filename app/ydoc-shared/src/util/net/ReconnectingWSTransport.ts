/**
 * This file is modified version of open-rpc/client-js WebSocketTransport implementation
 * (https://github.com/open-rpc/client-js/blob/master/src/transports/WebSocketTransport.ts)
 * which uses the automatically reconnecting websocket.
 */

import { WebSocketTransport } from '@open-rpc/client-js'
import WS from 'isomorphic-ws'
import { WebSocket } from 'partysocket'
import ReconnectingWebSocket, { Options, type WebSocketEventMap } from 'partysocket/ws'

export { ReconnectingWebSocket }

export interface AddEventListenerOptions {
  capture?: boolean
  once?: boolean
  passive?: boolean
  signal?: AbortSignal
}

export class ReconnectingWebSocketTransport extends WebSocketTransport {
  private _reconnectingConnection: ReconnectingWebSocket
  constructor(uri: string, wsOptions: Options = {}) {
    super(uri)
    this.uri = uri
    this._reconnectingConnection = new WebSocket(uri, undefined, {
      WebSocket: WS,
      ...wsOptions,
    })
    // Make sure that the WebSocketTransport implementation uses this version of socket.
    this.connection = this._reconnectingConnection as any
  }

  public reconnect() {
    this._reconnectingConnection.reconnect()
  }

  on<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
    options?: AddEventListenerOptions,
  ): void {
    this._reconnectingConnection.addEventListener(type, cb, options)
  }

  off<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
    options?: AddEventListenerOptions,
  ): void {
    this._reconnectingConnection.removeEventListener(type, cb, options)
  }
}
