import WebSocket from 'ws'
import { WebsocketProvider } from 'y-websocket'
import * as Y from 'yjs'

/**
 * Client that connects to the ydoc-server's inspect endpoint and
 * syncs the inspect Y.Doc via the standard Yjs sync protocol.
 */
export class InspectClient {
  readonly doc: Y.Doc
  readonly projectDoc: Y.Doc
  private provider: WebsocketProvider | null = null
  private projectProvider: WebsocketProvider | null = null
  private projectServerUrl: string | null = null
  private subdocProviders = new Map<string, WebsocketProvider>()
  onDisconnect: (() => void) | null = null

  /** Create a new InspectClient with fresh Y.Docs. */
  constructor() {
    this.doc = new Y.Doc()
    this.projectDoc = new Y.Doc()
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

  /**
   * Connect to the project document endpoint and start syncing the project Y.Doc.
   * The URL should point to the inspect/index room (e.g. ws://host:port/project/inspect/index).
   */
  connectProject(url: string): Promise<void> {
    const lastSlash = url.lastIndexOf('/')
    const serverUrl = url.slice(0, lastSlash)
    const roomname = url.slice(lastSlash + 1)
    this.projectServerUrl = serverUrl

    return new Promise((resolve, reject) => {
      let settled = false
      const provider = new WebsocketProvider(serverUrl, roomname, this.projectDoc, {
        WebSocketPolyfill: WebSocket as never,
        disableBc: true,
      })
      this.projectProvider = provider

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
          reject(new Error('Project doc connection closed before open'))
        } else {
          provider.destroy()
          this.projectProvider = null
        }
      }

      provider.on('status', onStatus)
      provider.on('connection-close', onClose)
    })
  }

  /**
   * Ensure a module subdoc is synced with the server by spinning up a dedicated
   * WebsocketProvider on `/project/inspect/<subdoc.guid>`. Subsequent calls for the
   * same subdoc await the existing provider's sync.
   */
  async loadSubdoc(subdoc: Y.Doc): Promise<void> {
    const existing = this.subdocProviders.get(subdoc.guid)
    if (existing) {
      if (existing.synced) return
      await waitForSynced(existing)
      return
    }
    if (!this.projectServerUrl) throw new Error('Project is not connected')
    const provider = new WebsocketProvider(this.projectServerUrl, subdoc.guid, subdoc, {
      WebSocketPolyfill: WebSocket as never,
      disableBc: true,
    })
    provider.maxBackoffTime = 0
    this.subdocProviders.set(subdoc.guid, provider)
    try {
      await waitForSynced(provider)
    } catch (e) {
      provider.destroy()
      this.subdocProviders.delete(subdoc.guid)
      throw e
    }
  }

  /** Close all WebSocket connections. */
  close(): void {
    for (const p of this.subdocProviders.values()) p.destroy()
    this.subdocProviders.clear()
    this.projectProvider?.destroy()
    this.projectProvider = null
    this.projectServerUrl = null
    this.provider?.destroy()
    this.provider = null
  }
}

function waitForSynced(provider: WebsocketProvider): Promise<void> {
  return new Promise((resolve, reject) => {
    const onSynced = (isSynced: boolean) => {
      if (!isSynced) return
      cleanup()
      resolve()
    }
    const onClose = () => {
      cleanup()
      reject(new Error('Connection closed before sync'))
    }
    const cleanup = () => {
      provider.off('synced', onSynced)
      provider.off('connection-close', onClose)
    }
    provider.on('synced', onSynced)
    provider.on('connection-close', onClose)
  })
}
