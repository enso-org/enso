import { YjsChannel, type TapDirection, type YjsChannelServer } from 'ydoc-channel'
import * as Y from 'yjs'
import type { JavaByteBuffer, JavaByteBufferClass } from './YjsBinaryChannel'
import { WSSharedDoc, YjsConnection, type YjsSocket } from './ydoc'

interface ChannelMeta {
  id: string
  channelName: string
  type: 'json' | 'data'
  createdAt: number
}

/** Converts a channel message to the log array storage type. */
type ToLog<TMessage, TStored> = (message: TMessage) => TStored
/** Converts a command array value back to the channel message type. */
type FromCmd<TMessage, TStored> = (value: TStored) => TMessage

/**
 * Manages channel inspection for debugging.
 *
 * When active, intercepts all YjsChannel message traffic and stores copies
 * in a dedicated inspect Y.Doc. An inspect client can sync this doc via
 * WebSocket to observe message history and send commands.
 */
export class InspectManager {
  private readonly inspectDoc: WSSharedDoc
  private readonly channelsMap: Y.Map<ChannelMeta>
  private readonly registeredChannels = new Map<string, { untap: () => void }>()
  private readonly byteBufferClass: JavaByteBufferClass
  private channelCounter = 0

  /** Create an {@link InspectManager}. */
  constructor(byteBufferClass: JavaByteBufferClass) {
    this.inspectDoc = new WSSharedDoc(false)
    this.channelsMap = this.inspectDoc.doc.getMap('channels')
    this.byteBufferClass = byteBufferClass
  }

  /** Wraps a JSON {@link YjsChannelServer} to intercept channel creation. */
  wrapJsonServer(delegate: YjsChannelServer<string>): YjsChannelServer<string> {
    return {
      onConnect: (channel: YjsChannel<string, unknown>) => {
        this.registerChannel(
          channel,
          'json',
          (m) => m,
          (v) => v,
        )
        delegate.onConnect(channel)
      },
    }
  }

  /** Wraps a binary {@link YjsChannelServer} to intercept channel creation. */
  wrapBinaryServer(delegate: YjsChannelServer<JavaByteBuffer>): YjsChannelServer<JavaByteBuffer> {
    return {
      onConnect: (channel: YjsChannel<JavaByteBuffer, unknown>) => {
        this.registerChannel(
          channel,
          'data',
          (m) => toBinary(m),
          (v) => this.fromBinary(v),
        )
        delegate.onConnect(channel)
      },
    }
  }

  /**
   * Handles a WebSocket connection from an inspect client.
   * Syncs the inspect Y.Doc to the client.
   */
  handleConnection(ws: YjsSocket): void {
    new YjsConnection(ws, this.inspectDoc)
  }

  private registerChannel<TMessage, TStored extends string | Uint8Array>(
    channel: YjsChannel<TMessage, unknown>,
    type: 'json' | 'data',
    toLog: ToLog<TMessage, TStored>,
    fromCmd: FromCmd<TMessage, TStored>,
  ): void {
    const id = `${type}-${this.channelCounter++}`

    this.channelsMap.set(id, {
      id,
      channelName: channel.channelName,
      type,
      createdAt: Date.now(),
    })

    const logArray = this.inspectDoc.doc.getArray<TStored>(`log:${id}`)
    const metaArray = this.inspectDoc.doc.getArray<string>(`meta:${id}`)

    const untap = channel.tap((message: TMessage, direction: TapDirection) => {
      const data = toLog(message)
      this.inspectDoc.doc.transact(() => {
        logArray.push([data])
        metaArray.push([JSON.stringify({ ts: Date.now(), dir: direction })])
      })
    })

    this.setupCommandForwarding(id, channel, fromCmd)
    this.setupReceiveForwarding(id, channel, fromCmd)
    this.registeredChannels.set(id, { untap })
  }

  private fromBinary(data: Uint8Array): JavaByteBuffer {
    const bb = this.byteBufferClass.allocateDirect(data.byteLength)
    const arr = new Uint8Array(new ArrayBuffer(bb))
    arr.set(data)
    return bb
  }

  private setupCommandForwarding<TMessage, TStored extends string | Uint8Array>(
    channelId: string,
    realChannel: YjsChannel<TMessage, unknown>,
    fromCmd: FromCmd<TMessage, TStored>,
  ): void {
    const cmdArray = this.inspectDoc.doc.getArray<TStored>(`snd:${channelId}`)
    const cmdSenderId = `inspect-snd-${channelId}`

    cmdArray.observe((event: Y.YArrayEvent<TStored>, transaction: Y.Transaction) => {
      if (transaction.origin === cmdSenderId) return

      const inserted: { index: number; value: TStored }[] = []
      let pos = 0
      for (const delta of event.changes.delta) {
        if (delta.retain) pos += delta.retain
        if (delta.insert) {
          const items = Array.isArray(delta.insert) ? delta.insert : [delta.insert]
          for (const item of items) {
            inserted.push({ index: pos, value: item })
            pos++
          }
        }
      }

      this.inspectDoc.doc.transact(() => {
        for (let i = inserted.length - 1; i >= 0; i--) {
          cmdArray.delete(inserted[i]!.index, 1)
        }
      }, cmdSenderId)

      for (const { value } of inserted) {
        try {
          realChannel.send(fromCmd(value))
        } catch (e) {
          console.error(`Failed to forward inspect command to ${channelId}:`, e)
        }
      }
    })
  }

  private setupReceiveForwarding<TMessage, TStored extends string | Uint8Array>(
    channelId: string,
    realChannel: YjsChannel<TMessage, unknown>,
    fromCmd: FromCmd<TMessage, TStored>,
  ): void {
    const rcvArray = this.inspectDoc.doc.getArray<TStored>(`rcv:${channelId}`)
    const rcvSenderId = `inspect-rcv-${channelId}`

    rcvArray.observe((event: Y.YArrayEvent<TStored>, transaction: Y.Transaction) => {
      if (transaction.origin === rcvSenderId) return

      const inserted: { index: number; value: TStored }[] = []
      let pos = 0
      for (const delta of event.changes.delta) {
        if (delta.retain) pos += delta.retain
        if (delta.insert) {
          const items = Array.isArray(delta.insert) ? delta.insert : [delta.insert]
          for (const item of items) {
            inserted.push({ index: pos, value: item })
            pos++
          }
        }
      }

      this.inspectDoc.doc.transact(() => {
        for (let i = inserted.length - 1; i >= 0; i--) {
          rcvArray.delete(inserted[i]!.index, 1)
        }
      }, rcvSenderId)

      for (const { value } of inserted) {
        try {
          const message = fromCmd(value)
          realChannel.notifyHandlers(message)
          realChannel.notifyTaps(message, 'receive')
        } catch (e) {
          console.error(`Failed to forward inspect receive to ${channelId}:`, e)
        }
      }
    })
  }
}

function toBinary(message: JavaByteBuffer): Uint8Array {
  return new Uint8Array(new ArrayBuffer(message))
}
