/**
 * This file is modified version of open-rpc/client-js WebSocketTransport implementation
 * (https://github.com/open-rpc/client-js/blob/master/src/transports/WebSocketTransport.ts)
 * which uses the automatically reconnecting websocket.
 */

import { ERR_UNKNOWN, JSONRPCError } from '@open-rpc/client-js/build/Error'
import {
  getBatchRequests,
  getNotifications,
  type JSONRPCRequestData,
} from '@open-rpc/client-js/build/Request'
import { Transport } from '@open-rpc/client-js/build/transports/Transport'
import WS from 'isomorphic-ws'
import { WebSocket } from 'partysocket'
import { type WebSocketEventMap } from 'partysocket/ws'

class ReconnectingWebSocketTransport extends Transport {
  public connection: WebSocket
  public uri: string

  constructor(uri: string) {
    super()
    this.uri = uri
    this.connection = new WebSocket(uri, undefined, { WebSocket: WS })
  }

  public connect(): Promise<any> {
    return new Promise<void>((resolve) => {
      this.connection.addEventListener('open', () => resolve(), { once: true })
      this.connection.addEventListener('message', ({ data }: { data: string }) => {
        this.transportRequestManager.resolveResponse(data)
      })
    })
  }

  public reconnect() {
    this.connection.reconnect()
  }

  public async sendData(data: JSONRPCRequestData, timeout: number | null = 5000): Promise<any> {
    let promise = this.transportRequestManager.addRequest(data, timeout)
    const notifications = getNotifications(data)
    try {
      this.connection.send(JSON.stringify(this.parseData(data)))
      this.transportRequestManager.settlePendingRequest(notifications)
    } catch (err) {
      const jsonError = new JSONRPCError((err as any).message, ERR_UNKNOWN, err)

      this.transportRequestManager.settlePendingRequest(notifications, jsonError)
      this.transportRequestManager.settlePendingRequest(getBatchRequests(data), jsonError)

      promise = Promise.reject(jsonError)
    }

    return promise
  }

  public close(): void {
    console.log(':) closing', this.uri)
    this.connection.close()
  }

  on<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
    options?: AddEventListenerOptions,
  ): void {
    this.connection.addEventListener(type, cb, options)
  }

  off<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
    options?: AddEventListenerOptions,
  ): void {
    this.connection.removeEventListener(type, cb, options)
  }
}

export default ReconnectingWebSocketTransport
