import * as map from 'lib0/map'
import { YjsChannel, type ChannelCodec, type YjsChannelServer } from 'ydoc-channel'
import * as Y from 'yjs'

/**
 * Represents a Java ByteBuffer instance accessed via GraalVM polyglot.
 *
 * This is a branded type alias for `number` that can be used directly with
 * ArrayBuffer constructor. The brand ensures type safety by preventing
 * accidental use of plain numbers where ByteBuffer is expected.
 */
export type JavaByteBuffer = number & { readonly __brand: 'JavaByteBuffer' }

/**
 * Represents the Java ByteBuffer class accessed via GraalVM polyglot.
 * Provides factory methods to create ByteBuffer instances.
 */
export interface JavaByteBufferClass {
  /** Allocates a new direct byte buffer with the given capacity */
  allocateDirect(capacity: number): JavaByteBuffer
}

/**
 * Codec that converts between Java ByteBuffer (external API) and Uint8Array (Y.Array storage).
 *
 * Exported for reuse by other binary-carrying channels (e.g. the visualization
 * data channel), which need the same polyglot-aware encoding but are wired up
 * outside of `YjsBinaryChannel` because they share a `Y.Array` with a paired
 * identity-coded endpoint on the ydoc-server side.
 */
export class JavaByteBufferCodec implements ChannelCodec<JavaByteBuffer, Uint8Array> {
  /** @param ByteBuffer Polyglot handle to `java.nio.ByteBuffer`. */
  constructor(private readonly ByteBuffer: JavaByteBufferClass) {}

  /** Wrap a Java `ByteBuffer` in a `Uint8Array` view without copying. */
  encode(message: JavaByteBuffer): Uint8Array {
    return new Uint8Array(new ArrayBuffer(message))
  }

  /** Allocate a direct Java `ByteBuffer` and copy the stored bytes into it. */
  decode(stored: Uint8Array): JavaByteBuffer {
    const bb = this.ByteBuffer.allocateDirect(stored.byteLength)
    const arr = new Uint8Array(new ArrayBuffer(bb))
    arr.set(stored)
    return bb
  }
}

/**
 * A {@link YjsChannel} for binary protocol communication with the Language Server.
 *
 * Extends YjsChannel to handle binary data by converting between JavaScript Uint8Array
 * and Java direct ByteBuffer. This enables efficient binary message transfer between
 * the Ydoc server (JavaScript) and the Language Server (Java/Scala).
 */
export class YjsBinaryChannel extends YjsChannel<JavaByteBuffer, Uint8Array> {
  private static channels = new Map<string, YjsBinaryChannel>()

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
    super(doc, channelName, new JavaByteBufferCodec(byteBuffer))
    server.onConnect(this)
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
}
