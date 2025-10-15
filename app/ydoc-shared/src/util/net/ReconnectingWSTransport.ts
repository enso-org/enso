/**
 * This file is modified version of open-rpc/client-js WebSocketTransport implementation
 * (https://github.com/open-rpc/client-js/blob/master/src/transports/WebSocketTransport.ts)
 * which uses the automatically reconnecting websocket.
 */

import { JSONRPCError } from '@open-rpc/client-js'
import { ERR_UNKNOWN } from '@open-rpc/client-js/build/Error.js'
import {
  getBatchRequests,
  getNotifications,
  type JSONRPCRequestData,
} from '@open-rpc/client-js/build/Request.js'
import { Transport } from '@open-rpc/client-js/build/transports/Transport.js'
import WS from 'modern-isomorphic-ws'
import type { Options } from 'partysocket/ws'
import ReconnectingWebSocket, { type WebSocketEventMap } from 'partysocket/ws'

export { ReconnectingWebSocket }

export interface AddEventListenerOptions {
  capture?: boolean
  once?: boolean
  passive?: boolean
  signal?: AbortSignal
}

/** A socket that automatically connects upon disconnect, for example after network issues. */
export class ReconnectingWebSocketTransport extends Transport {
  private socket: ReconnectingWebSocket
  /** Create a {@link ReconnectingWebSocketTransport}. */
  constructor(uri: string, wsOptions: Options = {}) {
    super()
    this.socket = new ReconnectingWebSocket(uri, undefined, {
      WebSocket: WS,
      ...wsOptions,
      startClosed: true,
    })
  }

  /**
   * Initiate socket connection to the server.
   */
  public connect(): Promise<void> {
    return new Promise((resolve, _reject) => {
      const onOpen = () => {
        this.off('open', onOpen)
        resolve()
      }
      this.on('open', onOpen)
      this.on('message', (message) => this.transportRequestManager.resolveResponse(message.data))
      this.socket.reconnect()
    })
  }

  /**
   *
   */
  public async sendData(data: JSONRPCRequestData, timeout: number | null = 5000): Promise<any> {
    let prom = this.transportRequestManager.addRequest(data, timeout)
    const notifications = getNotifications(data)
    try {
      this.socket.send(JSON.stringify(this.parseData(data)))
      this.transportRequestManager.settlePendingRequest(notifications)
    } catch (err) {
      const jsonError = new JSONRPCError((err as any).message, ERR_UNKNOWN, err)

      this.transportRequestManager.settlePendingRequest(notifications, jsonError)
      this.transportRequestManager.settlePendingRequest(getBatchRequests(data), jsonError)

      prom = Promise.reject(jsonError)
    }

    return prom
  }

  /** Close the underlying WebSocket. */
  public close(): void {
    this.socket.close()
  }

  /** Reconnect the underlying WebSocket. */
  public reconnect() {
    this.socket.reconnect()
  }

  /** Add an event listener to the underlying WebSocket. */
  on<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
    options?: AddEventListenerOptions,
  ): void {
    this.socket.addEventListener(type, cb, options)
  }

  /** Remove an event listener from the underlying WebSocket. */
  off<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
    options?: AddEventListenerOptions,
  ): void {
    this.socket.removeEventListener(type, cb, options)
  }
}
