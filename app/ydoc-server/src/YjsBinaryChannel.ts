import * as map from 'lib0/map'
import { YjsChannel, type MessageHandler, type YjsChannelCallbacks } from 'ydoc-channel'
import * as Y from 'yjs'

export class YjsBinaryChannel<T = unknown> extends YjsChannel<T> {
  private static channels = new Map<string, YjsBinaryChannel>()

  private readonly callbacks: YjsChannelCallbacks<T>
  private readonly ByteBuffer: any

  constructor(doc: Y.Doc, channelName: string, callbacks: YjsChannelCallbacks<T>, byteBuffer: any) {
    super(doc, channelName)
    console.log('new YjsBinaryChannel()')
    this.callbacks = callbacks
    this.ByteBuffer = byteBuffer
    try {
      this.callbacks.onConnect(this)
    } catch (e) {
      console.log('new YjsBinaryChannel onConnect err', e)
    }
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

  override send(message: any): void {
    console.log('DEBUG YjsBinaryChannel.send', message)
    const arr = new Uint8Array(new ArrayBuffer(message))
    super.send(arr as T)
  }

  override subscribe(handler: MessageHandler<T>): () => void {
    console.log('YjsBinaryChannel.subscribe', handler)
    const f = (contents: Uint8Array) => {
      console.log('YjsBinaryChannel.subscribe f', contents)
      const bb = this.ByteBuffer.allocateDirect(contents.byteLength)
      console.log('YjsBinaryChannel.subscribe f bb', bb)
      const arr = new Uint8Array(new ArrayBuffer(bb))
      arr.set(contents)
      return handler(bb)
    }
    return super.subscribe(f as MessageHandler<T>)
  }
}
