import WebSocket from 'ws'
import { WebsocketProvider } from 'y-websocket'
import * as Y from 'yjs'

/**
 * Client that connects to the ydoc-server's inspect endpoint and
 * syncs the inspect Y.Doc via the standard Yjs sync protocol.
 */
export class InspectClient {
  readonly doc: Y.Doc
  private provider: WebsocketProvider | null = null
  onDisconnect: (() => void) | null = null

  /** Create a new InspectClient with a fresh Y.Doc. */
  constructor() {
    this.doc = new Y.Doc()
  }

  /** Whether the WebSocket connection is currently active. */
  get connected(): boolean {
    return this.provider?.wsconnected ?? false
  }

  /** Connect to the inspect endpoint and start syncing the Y.Doc. */
  connect(url: string): Promise<void> {
    const lastSlash = url.lastIndexOf('/')
    const serverUrl = url.slice(0, lastSlash)
    const roomname = url.slice(lastSlash + 1)

    return new Promise((resolve, reject) => {
      let settled = false
      const provider = new WebsocketProvider(serverUrl, roomname, this.doc, {
        WebSocketPolyfill: WebSocket as never,
        disableBc: true,
      })
      this.provider = provider

      // Disable auto-reconnect. main.ts handles retry logic.
      provider.shouldConnect = false
      provider.maxBackoffTime = 0

      const onStatus = ({ status }: { status: string }) => {
        if (status === 'connected' && !settled) {
          settled = true
          resolve()
        }
      }

      const onClose = () => {
        provider.off('status', onStatus)
        provider.off('connection-close', onClose)
        if (!settled) {
          settled = true
          provider.destroy()
          reject(new Error('Connection closed before open'))
        } else {
          console.log('Disconnected from inspect server')
          provider.destroy()
          this.provider = null
          this.onDisconnect?.()
        }
      }

      provider.on('status', onStatus)
      provider.on('connection-close', onClose)
    })
  }

  /** Close the WebSocket connection. */
  close(): void {
    this.provider?.destroy()
    this.provider = null
  }
}
