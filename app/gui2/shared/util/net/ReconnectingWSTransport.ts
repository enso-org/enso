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
import { ObservableV2 } from 'lib0/observable'
import { WebSocket } from 'partysocket'

class ReconnectingWebSocketTransport extends Transport {
  public connection: WebSocket
  public uri: string
  public events: ObservableV2<{ closed(event: CloseEvent): void }>

  constructor(uri: string) {
    super()
    this.uri = uri
    this.connection = new WebSocket(uri, undefined, { WebSocket: WS, maxEnqueuedMessages: 0 })
    this.events = new ObservableV2()
  }
  public connect(): Promise<any> {
    return new Promise<void>((resolve) => {
      const cb = () => {
        this.connection.removeEventListener('open', cb)
        resolve()
      }
      this.connection.addEventListener('open', cb)
      this.connection.addEventListener('message', (message: { data: string }) => {
        const { data } = message
        this.transportRequestManager.resolveResponse(data)
      })
    })
  }

  public async sendData(data: JSONRPCRequestData, timeout: number | null = 5000): Promise<any> {
    let prom = this.transportRequestManager.addRequest(data, timeout)
    const notifications = getNotifications(data)
    try {
      this.connection.send(JSON.stringify(this.parseData(data)))
      this.transportRequestManager.settlePendingRequest(notifications)
    } catch (err) {
      const jsonError = new JSONRPCError((err as any).message, ERR_UNKNOWN, err)

      this.transportRequestManager.settlePendingRequest(notifications, jsonError)
      this.transportRequestManager.settlePendingRequest(getBatchRequests(data), jsonError)

      prom = Promise.reject(jsonError)
    }

    return prom
  }

  public close(): void {
    this.connection.close()
  }

  on<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
  ): void {
    this.connection.addEventListener(type, cb)
  }
  off<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
  ): void {
    this.connection.removeEventListener(type, cb)
  }
}

export default ReconnectingWebSocketTransport
