import {
  applyAwarenessUpdate,
  Awareness,
  encodeAwarenessUpdate,
  removeAwarenessStates,
} from 'y-protocols/awareness'
import { readSyncMessage, writeSyncStep1, writeUpdate } from 'y-protocols/sync'
import * as Y from 'yjs'

import * as decoding from 'lib0/decoding'
import * as encoding from 'lib0/encoding'
import { ObservableV2 } from 'lib0/observable.js'
import { WebSocket } from 'ws'
import { LanguageServerSession } from './languageServerSession'

const pingTimeout = 30000

const messageSync = 0
const messageAwareness = 1

interface AwarenessUpdate {
  added: number[]
  updated: number[]
  removed: number[]
}

type ConnectionId = YjsConnection | string

/**
 * A Yjs document that is shared over multiple websocket connections.
 */
export class WSSharedDoc {
  doc: Y.Doc
  /**
   * Maps from conn to set of controlled user ids. Delete all user ids from awareness when this conn
   * is closed.
   */
  conns: Map<ConnectionId, Set<number>>
  awareness: Awareness

  constructor(gc = true) {
    this.doc = new Y.Doc({ gc })
    // this.name = name
    this.conns = new Map()

    this.awareness = new Awareness(this.doc)
    this.awareness.setLocalState(null)

    this.awareness.on(
      'update',
      ({ added, updated, removed }: AwarenessUpdate, conn: ConnectionId | null) => {
        const changedClients = added.concat(updated, removed)
        if (conn !== null) {
          const connControlledIDs = this.conns.get(conn)
          if (connControlledIDs !== undefined) {
            added.forEach((clientID) => {
              connControlledIDs.add(clientID)
            })
            removed.forEach((clientID) => {
              connControlledIDs.delete(clientID)
            })
          }
        }
        // broadcast awareness update
        const encoder = encoding.createEncoder()
        encoding.writeVarUint(encoder, messageAwareness)
        encoding.writeVarUint8Array(encoder, encodeAwarenessUpdate(this.awareness, changedClients))
        this.broadcast(encoding.toUint8Array(encoder))
      },
    )
    this.doc.on('update', (update, origin) => this.updateHandler(update, origin))
  }

  broadcast(message: Uint8Array) {
    this.conns.forEach((_, conn) => {
      if (typeof conn !== 'string') {
        conn.send(message)
      }
    })
  }

  updateHandler(update: Uint8Array, _origin: any) {
    const encoder = encoding.createEncoder()
    encoding.writeVarUint(encoder, messageSync)
    writeUpdate(encoder, update)
    this.broadcast(encoding.toUint8Array(encoder))
  }
}

/**
 * Handle servicing incoming websocket connection listening for given document updates.
 * @param ws The newly connected websocket requesting Yjs document synchronization
 * @param lsUrl Address of the language server to connect to. Each unique language server address
 * will be assigned its own `DistributedProject` instance with a unique namespace of Yjs documents.
 * @param docName The name of the document to synchronize. When the document name is `index`, the
 * document is considered to be the root document of the `DistributedProject` data model.
 */
export function setupGatewayClient(ws: WebSocket, lsUrl: string, docName: string) {
  const lsSession = LanguageServerSession.get(lsUrl)
  const wsDoc = lsSession.getYDoc(docName)
  if (wsDoc == null) {
    console.log(`Document ${docName} not found in language server session ${lsUrl}`)
    ws.close()
    return
  }
  const connection = new YjsConnection(ws, wsDoc)

  const doClose = () => connection.close()
  lsSession.once('error', doClose)
  connection.once('close', async () => {
    lsSession.off('error', doClose)
    try {
      await lsSession.release()
    } catch (err) {
      console.error('Session release failed.\n', err)
    }
  })
}

class YjsConnection extends ObservableV2<{ close: () => void }> {
  ws: WebSocket
  wsDoc: WSSharedDoc
  constructor(ws: WebSocket, wsDoc: WSSharedDoc) {
    super()
    this.ws = ws
    this.wsDoc = wsDoc
    const isLoaded = wsDoc.conns.size > 0
    wsDoc.conns.set(this, new Set())
    ws.binaryType = 'arraybuffer'
    ws.on('message', (message: ArrayBuffer) => this.messageListener(new Uint8Array(message)))
    ws.on('close', () => this.close())

    if (!isLoaded) {
      wsDoc.doc.load()
    }

    this.initPing()
    this.sendSyncMessage()
  }

  private initPing() {
    // Check if connection is still alive
    let pongReceived = true
    const pingInterval = setInterval(() => {
      if (!pongReceived) {
        if (this.wsDoc.conns.has(this)) {
          this.close()
        }
        clearInterval(pingInterval)
      } else if (this.wsDoc.conns.has(this)) {
        pongReceived = false
        try {
          this.ws.ping()
        } catch (e) {
          this.close()
          clearInterval(pingInterval)
        }
      }
    }, pingTimeout)
    this.ws.on('close', () => clearInterval(pingInterval))
    this.ws.on('pong', () => {
      pongReceived = true
    })
  }

  sendSyncMessage() {
    const encoder = encoding.createEncoder()
    encoding.writeVarUint(encoder, messageSync)
    writeSyncStep1(encoder, this.wsDoc.doc)
    this.send(encoding.toUint8Array(encoder))
    const awarenessStates = this.wsDoc.awareness.getStates()
    if (awarenessStates.size > 0) {
      const encoder = encoding.createEncoder()
      encoding.writeVarUint(encoder, messageAwareness)
      encoding.writeVarUint8Array(
        encoder,
        encodeAwarenessUpdate(this.wsDoc.awareness, Array.from(awarenessStates.keys())),
      )
      this.send(encoding.toUint8Array(encoder))
    }
  }

  send(message: Uint8Array) {
    if (this.ws.readyState !== WebSocket.CONNECTING && this.ws.readyState !== WebSocket.OPEN) {
      this.close()
    }
    try {
      this.ws.send(message, (error) => {
        if (error != null) {
          this.close()
        }
      })
    } catch (e) {
      this.close()
    }
  }

  messageListener(message: Uint8Array) {
    try {
      const encoder = encoding.createEncoder()
      const decoder = decoding.createDecoder(message)
      const messageType = decoding.readVarUint(decoder)
      switch (messageType) {
        case messageSync:
          encoding.writeVarUint(encoder, messageSync)
          readSyncMessage(decoder, encoder, this.wsDoc.doc, this)

          // If the `encoder` only contains the type of reply message and no
          // message, there is no need to send the message. When `encoder` only
          // contains the type of reply, its length is 1.
          if (encoding.length(encoder) > 1) {
            this.send(encoding.toUint8Array(encoder))
          }
          break
        case messageAwareness: {
          const update = decoding.readVarUint8Array(decoder)
          applyAwarenessUpdate(this.wsDoc.awareness, update, this)
          break
        }
      }
    } catch (err) {
      console.error(err)
      this.wsDoc.doc.emit('error', [err])
    }
  }

  close() {
    const controlledIds = this.wsDoc.conns.get(this)
    this.wsDoc.conns.delete(this)
    if (controlledIds != null) {
      removeAwarenessStates(this.wsDoc.awareness, Array.from(controlledIds), null)
    }
    this.ws.close()
    this.emit('close', [])
    if (this.wsDoc.conns.size === 0) {
      this.wsDoc.doc.emit('unload', [])
    }
  }
}
