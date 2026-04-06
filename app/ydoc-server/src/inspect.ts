import { YjsChannel, type TapDirection, type YjsChannelServer } from 'ydoc-channel'
import * as Y from 'yjs'
import { WSSharedDoc, YjsConnection, type YjsSocket } from './ydoc'

interface ChannelMeta {
  id: string
  channelName: string
  type: 'json' | 'binary'
  createdAt: number
}

/** Interface for a Java ByteBuffer class providing factory methods. */
interface ByteBufferClass {
  allocateDirect(capacity: number): unknown
}

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
  private readonly registeredChannels = new Map<
    string,
    { channel: YjsChannel<any, any>; untap: () => void }
  >()
  private readonly byteBufferClass: ByteBufferClass
  private channelCounter = 0

  constructor(byteBufferClass: ByteBufferClass) {
    this.inspectDoc = new WSSharedDoc(false)
    this.channelsMap = this.inspectDoc.doc.getMap('channels')
    this.byteBufferClass = byteBufferClass
  }

  /**
   * Wraps a {@link YjsChannelServer} to intercept channel creation.
   * The returned server registers each channel for inspection before
   * delegating to the original.
   */
  wrapServer<T>(delegate: YjsChannelServer<T>, type: 'json' | 'binary'): YjsChannelServer<T> {
    return {
      onConnect: (channel: YjsChannel<T, unknown>) => {
        this.registerChannel(channel, type)
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

  private registerChannel(channel: YjsChannel<any, any>, type: 'json' | 'binary'): void {
    const id = `${type}-${this.channelCounter++}`

    this.channelsMap.set(id, {
      id,
      channelName: channel.channelName,
      type,
      createdAt: Date.now(),
    })

    const logArray = this.inspectDoc.doc.getArray<string | Uint8Array>(`log:${id}`)
    const metaArray = this.inspectDoc.doc.getArray<string>(`meta:${id}`)

    const untap = channel.tap((message: any, direction: TapDirection) => {
      const data = type === 'binary' ? toBinary(message) : message
      this.inspectDoc.doc.transact(() => {
        logArray.push([data])
        metaArray.push([JSON.stringify({ ts: Date.now(), dir: direction })])
      })
    })

    this.setupCommandForwarding(id, channel, type)
    this.registeredChannels.set(id, { channel, untap })
  }

  private fromBinary(data: Uint8Array): unknown {
    const bb = this.byteBufferClass.allocateDirect(data.byteLength)
    const arr = new Uint8Array(new ArrayBuffer(bb as never))
    arr.set(data)
    return bb
  }

  private setupCommandForwarding(
    channelId: string,
    realChannel: YjsChannel<any, any>,
    type: 'json' | 'binary',
  ): void {
    const cmdArray = this.inspectDoc.doc.getArray<string | Uint8Array>(`cmd:${channelId}`)
    const cmdSenderId = `inspect-cmd-${channelId}`

    cmdArray.observe((event: Y.YArrayEvent<string | Uint8Array>, transaction: Y.Transaction) => {
      if (transaction.origin === cmdSenderId) return

      const inserted: { index: number; value: string | Uint8Array }[] = []
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
          if (type === 'binary') {
            realChannel.send(this.fromBinary(value as Uint8Array))
          } else {
            realChannel.send(value)
          }
        } catch (e) {
          console.error(`Failed to forward inspect command to ${channelId}:`, e)
        }
      }
    })
  }
}

function toBinary(message: any): Uint8Array {
  if (message instanceof Uint8Array) return message
  // JavaByteBuffer from GraalVM polyglot branded number wrapping ArrayBuffer
  if (typeof message === 'number') return new Uint8Array(new ArrayBuffer(message))
  return new Uint8Array(0)
}
