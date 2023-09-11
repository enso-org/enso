import * as Y from 'yjs'
import { readSyncMessage, writeSyncStep1, writeUpdate } from 'y-protocols/sync'
import {
  Awareness,
  applyAwarenessUpdate,
  encodeAwarenessUpdate,
  removeAwarenessStates,
} from 'y-protocols/awareness'

import * as encoding from 'lib0/encoding'
import * as decoding from 'lib0/decoding'
import * as map from 'lib0/map'
import { WebSocket } from 'ws'

// const persistenceDir = process.env.YPERSISTENCE

// interface Persistence {
//   bindState: (string, WSSharedDoc) => void
//   writeState: (string, WSSharedDoc) => Promise<any>
//   provider: any
// }
// let persistence: Persistence | null = null
// if (typeof persistenceDir === 'string') {
//   console.info('Persisting documents to "' + persistenceDir + '"')
//   // @ts-ignore
//   const LeveldbPersistence = require('y-leveldb').LeveldbPersistence
//   const ldb = new LeveldbPersistence(persistenceDir)
//   persistence = {
//     provider: ldb,
//     bindState: async (docName, ydoc) => {
//       const persistedYdoc = await ldb.getYDoc(docName)
//       const newUpdates = Y.encodeStateAsUpdate(ydoc)
//       ldb.storeUpdate(docName, newUpdates)
//       Y.applyUpdate(ydoc, Y.encodeStateAsUpdate(persistedYdoc))
//       ydoc.on('update', (update) => {
//         ldb.storeUpdate(docName, update)
//       })
//     },
//     writeState: async (docName, ydoc) => {},
//   }
// }

// /**
//  * @param {{bindState: function(string,WSSharedDoc):void,
//  * writeState:function(string,WSSharedDoc):Promise<any>,provider:any}|null} persistence_
//  */
// exports.setPersistence = (persistence_) => {
//   persistence = persistence_
// }

// /**
//  * @return {null|{bindState: function(string,WSSharedDoc):void,
//  * writeState:function(string,WSSharedDoc):Promise<any>}|null} used persistence layer
//  */
// exports.getPersistence = () => persistence

const docs = new Map<string, WSSharedDoc>()

const messageSync = 0
const messageAwareness = 1
// const messageAuth = 2

const updateHandler = (update: Uint8Array, _origin: any, doc: WSSharedDoc) => {
  const encoder = encoding.createEncoder()
  encoding.writeVarUint(encoder, messageSync)
  writeUpdate(encoder, update)
  const message = encoding.toUint8Array(encoder)
  doc.conns.forEach((_, conn) => send(doc, conn, message))
}

interface AwarenessUpdate {
  added: number[]
  updated: number[]
  removed: number[]
}

type ConnectionId = WebSocket | string

class WSSharedDoc extends Y.Doc {
  name: string
  /**
   * Maps from conn to set of controlled user ids. Delete all user ids from awareness when this conn
   * is closed.
   */
  conns: Map<ConnectionId, Set<number>>
  awareness: Awareness

  constructor(name: string, gc = true) {
    super({ gc })
    this.name = name
    this.conns = new Map()

    this.awareness = new Awareness(this)
    this.awareness.setLocalState(null)

    this.awareness.on(
      'update',
      ({ added, updated, removed }: AwarenessUpdate, conn: ConnectionId | null) => {
        const changedClients = added.concat(updated, removed)
        if (conn !== null) {
          console.log('Conn:', conn.toString())
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
        const buff = encoding.toUint8Array(encoder)
        this.conns.forEach((_, c) => send(this, c, buff))
      },
    )
    this.on('update', updateHandler)
  }
}

/**
 * Gets a Y.Doc by name.
 */
export function getYDoc(docname: string, gc = true): WSSharedDoc {
  return map.setIfUndefined(docs, docname, () => {
    const doc = new WSSharedDoc(docname)
    doc.gc = gc
    // if (persistence !== null) {
    //   persistence.bindState(docname, doc)
    // }
    docs.set(docname, doc)
    return doc
  })
}

function messageListener(conn: WebSocket, doc: WSSharedDoc, message: Uint8Array) {
  try {
    const encoder = encoding.createEncoder()
    const decoder = decoding.createDecoder(message)
    const messageType = decoding.readVarUint(decoder)
    switch (messageType) {
      case messageSync:
        encoding.writeVarUint(encoder, messageSync)
        readSyncMessage(decoder, encoder, doc, conn)

        // If the `encoder` only contains the type of reply message and no
        // message, there is no need to send the message. When `encoder` only
        // contains the type of reply, its length is 1.
        if (encoding.length(encoder) > 1) {
          send(doc, conn, encoding.toUint8Array(encoder))
        }
        break
      case messageAwareness: {
        applyAwarenessUpdate(doc.awareness, decoding.readVarUint8Array(decoder), conn)
        break
      }
    }
  } catch (err) {
    console.error(err)
    doc.emit('error', [err])
  }
}

const closeConnection = (doc: WSSharedDoc, conn: WebSocket) => {
  const controlledIds = doc.conns.get(conn)
  doc.conns.delete(conn)
  if (controlledIds != null) {
    removeAwarenessStates(doc.awareness, Array.from(controlledIds), null)
    // if (doc.conns.size === 0 && persistence !== null) {
    //   // if persisted, we store state and destroy ydocument
    //   persistence.writeState(doc.name, doc).then(() => {
    //     doc.destroy()
    //   })
    //   docs.delete(doc.name)
    // }
  }
  conn.close()
}

const send = (doc: WSSharedDoc, conn: ConnectionId, m: Uint8Array) => {
  if (!(conn instanceof WebSocket)) return
  if (conn.readyState !== WebSocket.CONNECTING && conn.readyState !== WebSocket.OPEN) {
    closeConnection(doc, conn)
  }
  try {
    conn.send(
      m,
      /** @param {any} err */ (err) => {
        err != null && closeConnection(doc, conn)
      },
    )
  } catch (e) {
    closeConnection(doc, conn)
  }
}

const pingTimeout = 30000

export function setupWSConnection(ws: WebSocket, docName: string, gc = true) {
  console.log('DOC', docName)
  ws.binaryType = 'arraybuffer'
  const doc = getYDoc(docName, gc)
  doc.conns.set(ws, new Set())
  // listen and reply to events
  ws.on('message', (message: ArrayBuffer) => messageListener(ws, doc, new Uint8Array(message)))

  // Check if connection is still alive
  let pongReceived = true
  const pingInterval = setInterval(() => {
    if (!pongReceived) {
      if (doc.conns.has(ws)) {
        closeConnection(doc, ws)
      }
      clearInterval(pingInterval)
    } else if (doc.conns.has(ws)) {
      pongReceived = false
      try {
        ws.ping()
      } catch (e) {
        closeConnection(doc, ws)
        clearInterval(pingInterval)
      }
    }
  }, pingTimeout)
  ws.on('close', () => {
    closeConnection(doc, ws)
    clearInterval(pingInterval)
  })
  ws.on('pong', () => {
    pongReceived = true
  })
  // put the following in a variables in a block so the interval handlers don't keep in in
  // scope
  {
    // send sync step 1
    const encoder = encoding.createEncoder()
    encoding.writeVarUint(encoder, messageSync)
    writeSyncStep1(encoder, doc)
    send(doc, ws, encoding.toUint8Array(encoder))
    const awarenessStates = doc.awareness.getStates()
    if (awarenessStates.size > 0) {
      const encoder = encoding.createEncoder()
      encoding.writeVarUint(encoder, messageAwareness)
      encoding.writeVarUint8Array(
        encoder,
        encodeAwarenessUpdate(doc.awareness, Array.from(awarenessStates.keys())),
      )
      send(doc, ws, encoding.toUint8Array(encoder))
    }
  }
}
