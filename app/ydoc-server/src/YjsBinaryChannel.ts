import * as map from 'lib0/map'
import { YjsChannel, type MessageHandler, type YjsChannelServer } from 'ydoc-channel'
import * as Y from 'yjs'

/**
 * Represents a Java ByteBuffer instance accessed via GraalVM polyglot.
 *
 * This is a branded type alias for `number` that can be used directly with
 * ArrayBuffer constructor. The brand ensures type safety by preventing
 * accidental use of plain numbers where ByteBuffer is expected.
 */
type JavaByteBuffer = number & { readonly __brand: 'JavaByteBuffer' }

/**
 * Represents the Java ByteBuffer class accessed via GraalVM polyglot.
 * Provides factory methods to create ByteBuffer instances.
 */
interface JavaByteBufferClass {
  /** Allocates a new direct byte buffer with the given capacity */
  allocateDirect(capacity: number): JavaByteBuffer
}

/**
 * A {@link YjsChannel} for binary protocol communication with the Language Server.
 *
 * Extends YjsChannel to handle binary data by converting between JavaScript Uint8Array
 * and Java direct ByteBuffer. This enables efficient binary message transfer between
 * the Ydoc server (JavaScript) and the Language Server (Java/Scala).
 */
export class YjsBinaryChannel extends YjsChannel<unknown> {
  private static channels = new Map<string, YjsBinaryChannel>()

  private readonly server: YjsChannelServer<JavaByteBuffer>
  private readonly ByteBuffer: JavaByteBufferClass

  /**
   * @param doc - The Yjs document for CRDT-based message synchronization
   * @param channelName - Unique identifier for this channel
   * @param server - Language Server callbacks to notify on connection
   * @param byteBuffer - Java ByteBuffer class for allocating direct buffers
   */
  constructor(
    doc: Y.Doc,
    channelName: string,
    server: YjsChannelServer<JavaByteBuffer>,
    byteBuffer: JavaByteBufferClass,
  ) {
    super(doc, channelName)
    this.server = server
    this.ByteBuffer = byteBuffer
    this.server.onConnect(this)
  }

  /** Gets or creates a channel for the given name. Channels are cached and reused. */
  static get(
    doc: Y.Doc,
    channelName: string,
    server: YjsChannelServer<JavaByteBuffer>,
    byteBuffer: JavaByteBufferClass,
  ): YjsBinaryChannel {
    return map.setIfUndefined(YjsBinaryChannel.channels, channelName, () => {
      return new YjsBinaryChannel(doc, channelName, server, byteBuffer)
    })
  }

  /** Converts the Java ByteBuffer message to Uint8Array and sends through the channel. */
  override send(message: JavaByteBuffer): void {
    const arr = new Uint8Array(new ArrayBuffer(message))
    super.send(arr)
  }

  /** Wraps the handler to convert incoming Uint8Array to Java direct ByteBuffer. */
  override subscribe(handler: MessageHandler<JavaByteBuffer>): () => void {
    const f = (message: unknown) => {
      const contents = message as Uint8Array
      const bb = this.ByteBuffer.allocateDirect(contents.byteLength)
      const arr = new Uint8Array(new ArrayBuffer(bb))
      arr.set(contents)
      return handler(bb)
    }
    return super.subscribe(f)
  }
}
