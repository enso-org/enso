import * as map from 'lib0/map'
import { YjsChannel, type MessageHandler, type YjsChannelCallbacks } from 'ydoc-channel'
import * as Y from 'yjs'

/**
 * A {@link YjsChannel} for binary protocol communication with the Language Server.
 *
 * Extends YjsChannel to handle binary data by converting between JavaScript Uint8Array
 * and Java direct ByteBuffer. This enables efficient binary message transfer between
 * the Ydoc server (JavaScript) and the Language Server (Java/Scala).
 */
export class YjsBinaryChannel<T = unknown> extends YjsChannel<T> {
  private static channels = new Map<string, YjsBinaryChannel>()

  private readonly callbacks: YjsChannelCallbacks<T>
  private readonly ByteBuffer: any

  /**
   * @param doc - The Yjs document for CRDT-based message synchronization
   * @param channelName - Unique identifier for this channel
   * @param callbacks - Language Server callbacks to notify on connection
   * @param byteBuffer - Java ByteBuffer class for allocating direct buffers
   */
  constructor(doc: Y.Doc, channelName: string, callbacks: YjsChannelCallbacks<T>, byteBuffer: any) {
    super(doc, channelName)
    this.callbacks = callbacks
    this.ByteBuffer = byteBuffer
    this.callbacks.onConnect(this)
  }

  /** Gets or creates a channel for the given name. Channels are cached and reused. */
  static get(
    doc: Y.Doc,
    channelName: string,
    callbacks: YjsChannelCallbacks,
    byteBuffer: any,
  ): YjsBinaryChannel {
    return map.setIfUndefined(YjsBinaryChannel.channels, channelName, () => {
      return new YjsBinaryChannel(doc, channelName, callbacks, byteBuffer)
    })
  }

  /** Converts the message to Uint8Array and sends through the channel. */
  override send(message: any): void {
    const arr = new Uint8Array(new ArrayBuffer(message))
    super.send(arr as T)
  }

  /** Wraps the handler to convert incoming Uint8Array to Java direct ByteBuffer. */
  override subscribe(handler: MessageHandler<T>): () => void {
    const f = (contents: Uint8Array) => {
      const bb = this.ByteBuffer.allocateDirect(contents.byteLength)
      const arr = new Uint8Array(new ArrayBuffer(bb))
      arr.set(contents)
      return handler(bb)
    }
    return super.subscribe(f as MessageHandler<T>)
  }
}
