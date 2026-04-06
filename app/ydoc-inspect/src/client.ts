import * as decoding from 'lib0/decoding'
import * as encoding from 'lib0/encoding'
import WebSocket from 'ws'
import { applyAwarenessUpdate, Awareness } from 'y-protocols/awareness'
import { readSyncMessage, writeSyncStep1, writeUpdate } from 'y-protocols/sync'
import * as Y from 'yjs'

const messageSync = 0
const messageAwareness = 1

/**
 * Client that connects to the ydoc-server's inspect endpoint and
 * syncs the inspect Y.Doc via the standard Yjs sync protocol.
 */
export class InspectClient {
  readonly doc: Y.Doc
  readonly awareness: Awareness
  private ws: WebSocket | null = null
  private _connected = false
  onDisconnect: (() => void) | null = null

  /** Create an {@link InspectClient}. */
  constructor() {
    this.doc = new Y.Doc()
    this.awareness = new Awareness(this.doc)
    this.awareness.setLocalState(null)

    this.doc.on('update', (update: Uint8Array, origin: unknown) => {
      if (origin !== 'remote' && this.ws?.readyState === WebSocket.OPEN) {
        const encoder = encoding.createEncoder()
        encoding.writeVarUint(encoder, messageSync)
        writeUpdate(encoder, update)
        this.ws.send(encoding.toUint8Array(encoder))
      }
    })
  }

  /** Whether the client is currently connected. */
  get connected(): boolean {
    return this._connected
  }

  /** Connect to the inspect endpoint and start syncing the Y.Doc. */
  connect(url: string): Promise<void> {
    return new Promise((resolve, reject) => {
      let settled = false
      const ws = new WebSocket(url)
      ws.binaryType = 'arraybuffer'
      this.ws = ws

      ws.on('open', () => {
        settled = true
        this._connected = true
        const encoder = encoding.createEncoder()
        encoding.writeVarUint(encoder, messageSync)
        writeSyncStep1(encoder, this.doc)
        ws.send(encoding.toUint8Array(encoder))
        resolve()
      })

      ws.on('message', (data: ArrayBuffer) => {
        const message = new Uint8Array(data)
        const decoder = decoding.createDecoder(message)
        const messageType = decoding.readVarUint(decoder)

        switch (messageType) {
          case messageSync: {
            const encoder = encoding.createEncoder()
            encoding.writeVarUint(encoder, messageSync)
            readSyncMessage(decoder, encoder, this.doc, 'remote')
            if (encoding.length(encoder) > 1) {
              ws.send(encoding.toUint8Array(encoder))
            }
            break
          }
          case messageAwareness: {
            const update = decoding.readVarUint8Array(decoder)
            applyAwarenessUpdate(this.awareness, update, 'remote')
            break
          }
        }
      })

      ws.on('close', () => {
        const wasConnected = this._connected
        this._connected = false
        if (!settled) {
          settled = true
          reject(new Error('Connection closed before open'))
        } else if (wasConnected) {
          console.log('Disconnected from inspect server')
          this.onDisconnect?.()
        }
      })

      ws.on('error', () => {
        // Connection errors are handled via the 'close' event which always
        // follows 'error'. Suppress here to avoid unhandled rejection.
      })
    })
  }

  /** Close the WebSocket connection. */
  close(): void {
    this.ws?.close()
    this.ws = null
  }
}
