/* eslint-env browser */

/* The MIT License (MIT)
 *
 * Copyright (c) 2019 Kevin Jahns <kevin.jahns@protonmail.com>.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/**
 * Tiny websocket connection handler.
 *
 * Implements exponential backoff reconnects, ping/pong, and a nice event system using [lib0/observable].
 *
 * @module websocket
 */

import * as math from 'lib0/math'
import { ObservableV2 } from 'lib0/observable'
import * as time from 'lib0/time'

const reconnectTimeoutBase = 1200
const maxReconnectTimeout = 2500
// @todo - this should depend on awareness.outdatedTime
const messageReconnectTimeout = 30000

const setupWS = (wsclient: WebsocketClient) => {
  if (wsclient.shouldConnect && wsclient.ws === null) {
    // @ts-ignore I don't know why `lib` is misconfigured.
    const websocket = new WebSocket(wsclient.url)
    const binaryType = wsclient.binaryType
    let pingTimeout: any = null
    if (binaryType) {
      websocket.binaryType = binaryType
    }
    wsclient.ws = websocket
    wsclient.connecting = true
    wsclient.connected = false
    websocket.onmessage = (event: { data: string | ArrayBuffer | Blob }) => {
      wsclient.lastMessageReceived = time.getUnixTime()
      const data = event.data
      const message = typeof data === 'string' ? JSON.parse(data) : data
      if (wsclient.sendPings && message && message.type === 'pong') {
        clearTimeout(pingTimeout)
        pingTimeout = setTimeout(sendPing, messageReconnectTimeout / 2)
      }
      wsclient.emit('message', [message, wsclient])
    }
    const onclose = (error: unknown) => {
      if (wsclient.ws !== null) {
        wsclient.ws = null
        wsclient.connecting = false
        if (wsclient.connected) {
          wsclient.connected = false
          wsclient.emit('disconnect', [{ type: 'disconnect', error }, wsclient])
        } else {
          wsclient.unsuccessfulReconnects++
        }
        // Start with no reconnect timeout and increase timeout by
        // log10(wsUnsuccessfulReconnects).
        // The idea is to increase reconnect timeout slowly and have no reconnect
        // timeout at the beginning (log(1) = 0)
        setTimeout(
          setupWS,
          math.min(
            math.log10(wsclient.unsuccessfulReconnects + 1) * reconnectTimeoutBase,
            maxReconnectTimeout,
          ),
          wsclient,
        )
      }
      clearTimeout(pingTimeout)
    }
    const sendPing = () => {
      if (wsclient.sendPings && wsclient.ws === websocket) {
        wsclient.send({
          type: 'ping',
        })
      }
    }
    websocket.onclose = () => onclose(null)
    websocket.onerror = (error: unknown) => onclose(error)
    websocket.onopen = () => {
      wsclient.lastMessageReceived = time.getUnixTime()
      wsclient.connecting = false
      wsclient.connected = true
      wsclient.unsuccessfulReconnects = 0
      wsclient.emit('connect', [{ type: 'connect' }, wsclient])
      // set ping
      pingTimeout = setTimeout(sendPing, messageReconnectTimeout / 2)
    }
  }
}

type WebsocketEvents = {
  connect: (payload: { type: 'connect' }, self: WebsocketClient) => void
  disconnect: (payload: { type: 'disconnect'; error: unknown }, self: WebsocketClient) => void
  message: (payload: {} | ArrayBuffer | Blob, self: WebsocketClient) => void
}

export class WebsocketClient extends ObservableV2<WebsocketEvents> {
  ws: any
  binaryType
  sendPings
  connected
  connecting
  unsuccessfulReconnects
  lastMessageReceived
  shouldConnect
  protected _checkInterval
  constructor(
    public url: string,
    {
      binaryType,
      sendPings,
    }: { binaryType?: 'arraybuffer' | 'blob' | null; sendPings?: boolean } = {},
  ) {
    super()
    this.ws = null
    this.binaryType = binaryType || null
    this.sendPings = sendPings ?? true
    this.connected = false
    this.connecting = false
    this.unsuccessfulReconnects = 0
    this.lastMessageReceived = 0
    /** Whether to connect to other peers or not */
    this.shouldConnect = true
    this._checkInterval = this.sendPings
      ? setInterval(() => {
          if (
            this.connected &&
            messageReconnectTimeout < time.getUnixTime() - this.lastMessageReceived
          ) {
            // no message received in a long time - not even your own awareness
            // updates (which are updated every 15 seconds)
            this.ws.close()
          }
        }, messageReconnectTimeout / 2)
      : 0
    setupWS(this)
  }

  send(message: {} | ArrayBuffer | Blob) {
    if (this.ws) {
      const encoded =
        message instanceof ArrayBuffer || message instanceof Blob
          ? message
          : JSON.stringify(message)
      this.ws.send(encoded)
    }
  }

  destroy() {
    clearInterval(this._checkInterval)
    this.disconnect()
    super.destroy()
  }

  disconnect() {
    this.shouldConnect = false
    if (this.ws !== null) {
      this.ws.close()
    }
  }

  connect() {
    this.shouldConnect = true
    if (!this.connected && this.ws === null) {
      setupWS(this)
    }
  }
}
