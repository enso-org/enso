import { ObservableV2 } from 'lib0/observable'
import * as Y from 'yjs'

interface AddEventListenerOptions {
  capture?: boolean
  once?: boolean
  passive?: boolean
  signal?: AbortSignal
}

/**
 * Message handler callback type.
 */
export type MessageHandler<T = unknown> = (message: T) => void

/**
 * Direction of a tapped message.
 */
export type TapDirection = 'send' | 'receive'

/**
 * Non-consuming message observer callback type.
 * Receives a copy of every message passing through the channel.
 */
export type TapHandler<T> = (message: T, direction: TapDirection) => void

/**
 * Codec for converting between the external message type and the internal storage type.
 */
export interface ChannelCodec<TMessage, TStored> {
  /** Encode a message for storage in the Y.Array. */
  encode(message: TMessage): TStored
  /** Decode a stored value back into a message. */
  decode(stored: TStored): TMessage
}

/** Identity codec that passes values through unchanged. */
export const identityCodec: ChannelCodec<any, any> = {
  encode: (message) => message,
  decode: (stored) => stored,
}

/**
 * Callback interface for receiving newly established {@link YjsChannel} connections.
 *
 * Invoked when a WebSocket client connects, providing a channel for bidirectional communication.
 */
export interface YjsChannelServer<T = unknown> {
  /**
   * Called when a new channel is established.
   * @param channel - The newly connected channel
   */
  onConnect(channel: YjsChannel<T, unknown>): void
}

/**
 * ObservableV2-compatible event handlers for WebSocketEventMap.
 */
type WebSocketEventHandlers = {
  [K in keyof WebSocketEventMap]: (event: WebSocketEventMap[K]) => void
}

/**
 * A bidirectional communication channel backed by Y.Array CRDT.
 *
 * Messages are stored in a shared Y.Array, enabling reliable cross-runtime communication.
 * Each sender has a unique ID used as transaction origin to filter out self-sent messages.
 * Implements WebSocket-like event API for compatibility with existing code.
 */
export class YjsChannel<
  TMessage = unknown,
  TStored = TMessage,
> extends ObservableV2<WebSocketEventHandlers> {
  readonly channelName: string
  private readonly senderId: string
  private readonly doc: Y.Doc
  private readonly array: Y.Array<TStored>
  private readonly handlers: Set<MessageHandler<TMessage>> = new Set()
  private readonly tapHandlers: Set<TapHandler<TMessage>> = new Set()
  private readonly observeHandler: (event: Y.YArrayEvent<TStored>, tr: Y.Transaction) => void
  private readonly codec: ChannelCodec<TMessage, TStored>

  /**
   * Creates a new YjsChannel.
   * @param doc - The shared Y.Doc document
   * @param channelName - The name of the channel (used to get/create the Y.Array)
   * @param codec - Optional codec for converting between message and storage types
   */
  constructor(
    doc: Y.Doc,
    channelName: string,
    codec: ChannelCodec<TMessage, TStored> = identityCodec,
  ) {
    super()
    this.channelName = channelName
    this.senderId = crypto.randomUUID()
    this.doc = doc
    this.array = doc.getArray<TStored>(channelName)
    this.codec = codec

    this.observeHandler = (event: Y.YArrayEvent<TStored>, transaction: Y.Transaction) => {
      // Only notify handlers if the message is from another sender
      if (transaction.origin !== this.senderId) {
        const hasHandlers = this.handlers.size > 0 || this.hasActiveEventListeners()

        // If no handlers or taps are subscribed, leave items in the array for later processing.
        // This handles the race condition where messages arrive before handlers are attached.
        if (!hasHandlers && this.tapHandlers.size === 0) {
          return
        }

        // Compute indices and values of remotely-inserted items from the delta.
        // Delta positions refer to the post-update array state:
        //   retain N: skip N unchanged items (advances position)
        //   insert:   new items at current position (advances position)
        //   delete N: removed items (does NOT advance position in new state)
        const inserted: { index: number; value: TStored }[] = []
        let pos = 0
        for (const delta of event.changes.delta) {
          if (delta.retain) {
            pos += delta.retain
          }
          if (delta.insert) {
            const items = Array.isArray(delta.insert) ? delta.insert : [delta.insert]
            for (const item of items) {
              inserted.push({ index: pos, value: item })
              pos++
            }
          }
        }

        // Only consume (delete) messages when regular handlers are present.
        // When only taps exist, leave items for later handler subscription.
        if (hasHandlers) {
          doc.transact(() => {
            // Delete the processed items in reverse index order to preserve correct positions
            for (let i = inserted.length - 1; i >= 0; i--) {
              this.array.delete(inserted[i]!.index, 1)
            }
          }, this.senderId)
        }

        for (const { value } of inserted) {
          const decoded = this.codec.decode(value)
          if (hasHandlers) {
            this.notifyHandlers(decoded)
          }
          this.notifyTaps(decoded, 'receive')
        }
      }
    }

    this.array.observe(this.observeHandler)
  }

  /**
   * Sends a message to the channel.
   * @param message - The message to send
   */
  send(message: TMessage): void {
    this.doc.transact(() => this.array.push([this.codec.encode(message)]), this.senderId)
    this.notifyTaps(message, 'send')
  }

  /**
   * Subscribes to messages received from other parties.
   *
   * If there are already messages in the array (e.g. sent before this subscription),
   * they will be delivered to the handler immediately and removed from the array.
   * @param handler - The callback to invoke when a message is received
   * @returns A function to unsubscribe the handler
   */
  subscribe(handler: MessageHandler<TMessage>): () => void {
    this.handlers.add(handler)

    // Process any existing items in the array that arrived before subscription
    // This handles the race condition where messages arrive before observers are attached
    if (this.array.length > 0) {
      this.doc.transact(() => {
        while (this.array.length > 0) {
          const item = this.codec.decode(this.array.get(0))
          try {
            handler(item)
          } catch (e) {
            this.emitError(new Error(`Failed to handle existing message: ${e}`, { cause: e }))
          }
          this.array.delete(0)
        }
      }, this.senderId)
    }

    return () => {
      this.handlers.delete(handler)
    }
  }

  /**
   * Registers a non-consuming observer that receives copies of all messages
   * passing through the channel (both sent and received).
   * @param handler - The callback to invoke with each message and its direction
   * @returns A function to unsubscribe the tap handler
   */
  tap(handler: TapHandler<TMessage>): () => void {
    this.tapHandlers.add(handler)
    return () => {
      this.tapHandlers.delete(handler)
    }
  }

  /**
   * Removes all message handlers and stops observing the Y.Array.
   */
  close(): void {
    this.array.unobserve(this.observeHandler)
    this.handlers.clear()
    this.tapHandlers.clear()
    this.emitClose()
  }

  /**
   * Add an event listener to the channel (alias for addEventListener).
   */
  override on<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (event: WebSocketEventMap[K]) => void,
    options?: AddEventListenerOptions,
  ): any {
    // If subscribing to 'open' event, call the callback immediately
    // since the channel is always open after creation
    if (type === 'open') {
      try {
        cb(new Event('open') as WebSocketEventMap[K])
      } catch (e) {
        this.emitError(new Error(`YjsChannel error handling open event ${e}`, { cause: e }))
      }
      // Don't add to listeners if 'once' option is set
      if (options?.once) {
        return cb
      }
    }

    // If subscribing to 'message' event, process any existing items in the array.
    if (type === 'message') {
      // Process any existing items in the array that arrived before subscription.
      // This handles the race condition where messages arrive before observers are attached.
      if (this.array.length > 0) {
        this.doc.transact(() => {
          while (this.array.length > 0) {
            const item = this.codec.decode(this.array.get(0))
            const messageEvent = { data: item } as MessageEvent
            try {
              cb(messageEvent as WebSocketEventMap[K])
            } catch (e) {
              this.emitError(new Error(`Failed to handle existing message: ${e}`, { cause: e }))
            }
            this.array.delete(0)
          }
        }, this.senderId)
      }
    }

    if (options?.once) {
      return super.once(type, cb as any)
    } else {
      return super.on(type, cb as any)
    }
  }

  /**
   * Remove an event listener from the channel (alias for removeEventListener).
   */
  override off<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (event: WebSocketEventMap[K]) => void,
    _options?: AddEventListenerOptions,
  ): void {
    super.off(type, cb as any)
  }

  /**
   * WebSocket-compatible addEventListener method.
   * Add an event listener to the channel.
   */
  addEventListener<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (event: WebSocketEventMap[K]) => void,
    options?: AddEventListenerOptions,
  ): void {
    this.on(type, cb, options)
  }

  /**
   * WebSocket-compatible removeEventListener method.
   * Remove an event listener from the channel.
   */
  removeEventListener<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (event: WebSocketEventMap[K]) => void,
    options?: AddEventListenerOptions,
  ): void {
    this.off(type, cb, options)
  }

  /**
   * Notifies all subscribed handlers with the received message.
   * Used by {@link InspectManager} to inject messages. Not part of the public API.
   * @internal
   */
  notifyHandlers(message: TMessage): void {
    // Create a MessageEvent-like object for WebSocket compatibility
    const messageEvent = { data: message } as MessageEvent

    // Emit event for addEventListener listeners
    super.emit('message', [messageEvent])

    // Call legacy subscribe handlers for backward compatibility
    for (const handler of this.handlers) {
      try {
        handler(message)
      } catch (e) {
        console.error('Failed to handle message', message, e)
        this.emitError(new Error(`Failed to handle message: ${message}`, { cause: e }))
      }
    }
  }

  /**
   * Notifies all tap handlers with the message and direction.
   * Used by {@link InspectManager} to inject messages. Not part of the public API.
   * @internal
   */
  notifyTaps(message: TMessage, direction: TapDirection): void {
    for (const handler of this.tapHandlers) {
      try {
        handler(message, direction)
      } catch (e) {
        console.error('Tap handler error', e)
      }
    }
  }

  /**
   * Returns true if there are active 'message' event listeners registered via on/addEventListener.
   * Queries the ObservableV2 internal observer map for a live count.
   */
  private hasActiveEventListeners(): boolean {
    const observers = (this as any)._observers as
      | Map<string, Set<(...args: any[]) => void>>
      | undefined
    return (observers?.get('message')?.size ?? 0) > 0
  }

  /**
   * Emit a 'close' event to signal the channel is closed.
   */
  private emitClose(): void {
    super.emit('close', [new CloseEvent('close')])
  }

  /**
   * Emit an 'error' event to signal an error occurred.
   */
  private emitError(error?: Error): void {
    const errorEvent = new Event('error')
    if (error) {
      ;(errorEvent as any).error = error
    }
    super.emit('error', [errorEvent])
  }
}
