import * as map from 'lib0/map'
import { YjsChannel, type MessageHandler, type YjsChannelCallbacks } from 'ydoc-channel'
import * as Y from 'yjs'

/**
 * A Yjs channel that handles binary data communication using ByteBuffer.
 * Extends YjsChannel to provide binary message encoding/decoding capabilities.
 */
export class YjsBinaryChannel<T = unknown> extends YjsChannel<T> {
  private static channels = new Map<string, YjsBinaryChannel>()

  private readonly callbacks: YjsChannelCallbacks<T>
  private readonly ByteBuffer: any

  /**
   * Creates a new YjsBinaryChannel instance.
   * @param doc - The Yjs document to synchronize
   * @param channelName - The name of the channel
   * @param callbacks - Callbacks for channel lifecycle events
   * @param byteBuffer - Java ByteBuffer class
   */
  constructor(doc: Y.Doc, channelName: string, callbacks: YjsChannelCallbacks<T>, byteBuffer: any) {
    super(doc, channelName)
    this.callbacks = callbacks
    this.ByteBuffer = byteBuffer
    this.callbacks.onConnect(this)
  }

  /** Get a {@link YjsBinaryChannel}. */
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

  /**
   * Sends a message through the channel.
   * Converts the message to a Uint8Array before sending.
   * @param message - The message to send
   */
  override send(message: any): void {
    const arr = new Uint8Array(new ArrayBuffer(message))
    super.send(arr as T)
  }

  /**
   * Subscribes to incoming messages on the channel.
   * Converts incoming Uint8Array messages to Java ByteBuffer before passing to the handler.
   * @param handler - The message handler function
   * @returns A function to unsubscribe from the channel
   */
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
