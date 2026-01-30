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
 * Callback interface for receiving newly established {@link YjsChannel} connections.
 *
 * Invoked when a WebSocket client connects, providing a channel for bidirectional communication.
 */
export interface YjsChannelCallbacks<T = unknown> {
  /**
   * Called when a new channel is established.
   * @param channel - The newly connected channel
   */
  onConnect(channel: YjsChannel<T>): void
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
export class YjsChannel<T = unknown> extends ObservableV2<WebSocketEventHandlers> {
  private readonly senderId: string
  private readonly doc: Y.Doc
  private readonly array: Y.Array<T>
  private readonly handlers: Set<MessageHandler<T>> = new Set()
  private readonly observeHandler: (event: Y.YArrayEvent<T>, tr: Y.Transaction) => void
  private hasMessageListeners = false

  /**
   * Creates a new YjsChannel.
   * @param doc - The shared Y.Doc document
   * @param channelName - The name of the channel (used to get/create the Y.Array)
   */
  constructor(doc: Y.Doc, channelName: string) {
    super()
    this.senderId = crypto.randomUUID()
    this.doc = doc
    this.array = doc.getArray<T>(channelName)

    this.observeHandler = (event: Y.YArrayEvent<T>, transaction: Y.Transaction) => {
      // Only notify handlers if the message is from another sender
      if (transaction.origin !== this.senderId) {
        // If no handlers are subscribed, leave items in the array for later processing.
        // This handles the race condition where messages arrive before handlers are attached.
        if (this.handlers.size === 0 && !this.hasMessageListeners) {
          return
        }
        doc.transact(() => {
          // Process all added items
          for (const delta of event.changes.delta) {
            if (delta.insert) {
              const items = Array.isArray(delta.insert) ? delta.insert : [delta.insert]
              for (const item of items) {
                this.notifyHandlers(item)
                this.array.delete(0)
              }
            }
          }
        }, this.senderId)
      }
    }

    this.array.observe(this.observeHandler)
  }

  /**
   * Sends a message to the channel.
   * @param message - The message to send
   */
  send(message: T): void {
    this.doc.transact(() => this.array.push([message]), this.senderId)
  }

  /**
   * Subscribes to messages received from other parties.
   * @param handler - The callback to invoke when a message is received
   * @returns A function to unsubscribe the handler
   */
  subscribe(handler: MessageHandler<T>): () => void {
    this.handlers.add(handler)

    // Process any existing items in the array that arrived before subscription
    // This handles the race condition where messages arrive before observers are attached
    if (this.array.length > 0) {
      this.doc.transact(() => {
        while (this.array.length > 0) {
          const item = this.array.get(0)
          try {
            handler(item)
          } catch (e) {
            const error = new Error(`Failed to handle existing message: ${e}`)
            ;(error as any).target = e
            this.emitError(error)
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
   * Removes all message handlers and stops observing the Y.Array.
   */
  close(): void {
    this.array.unobserve(this.observeHandler)
    this.handlers.clear()
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
        const error = new Error(`YjsChannel error handling open event ${e}`)
        ;(error as any).target = e
        this.emitError(error)
      }
      // Don't add to listeners if 'once' option is set
      if (options?.once) {
        return cb
      }
    }

    // If subscribing to 'message' event, mark that we have listeners and process existing items.
    if (type === 'message') {
      this.hasMessageListeners = true

      // Process any existing items in the array that arrived before subscription.
      // This handles the race condition where messages arrive before observers are attached.
      if (this.array.length > 0) {
        this.doc.transact(() => {
          while (this.array.length > 0) {
            const item = this.array.get(0)
            const messageEvent = { data: item } as MessageEvent
            try {
              cb(messageEvent as WebSocketEventMap[K])
            } catch (e) {
              const error = new Error(`Failed to handle existing message: ${e}`)
              ;(error as any).target = e
              this.emitError(error)
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
   */
  protected notifyHandlers(message: any): void {
    // Create a MessageEvent-like object for WebSocket compatibility
    const messageEvent = { data: message } as MessageEvent

    // Emit event for addEventListener listeners
    super.emit('message', [messageEvent])

    // Call legacy subscribe handlers for backward compatibility
    for (const handler of this.handlers) {
      try {
        handler(message)
      } catch (e) {
        const error = new Error(`Failed to handle message: ${message}`)
        ;(error as any).target = e
        this.emitError(error)
      }
    }
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
