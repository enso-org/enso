import * as Y from 'yjs'

/**
 * A message in the channel.
 */
interface ChannelMessage<T = unknown> {
  /** Unique ID of the sender. */
  senderId: string
  /** The message payload. */
  payload: T
}

/**
 * Message handler callback type.
 */
export type MessageHandler<T = unknown> = (message: T) => void

/**
 * Callbacks for YjsChannel lifecycle events.
 */
export interface YjsChannelCallbacks<T = unknown> {
  /**
   * Called when the channel is connected and ready to use.
   * @param channel - The connected YjsChannel instance
   */
  onConnect(channel: YjsChannel): void

  /**
   * Called when a message is received from another party.
   * @param message - The received message payload
   */
  onMessage(message: T): void
}

/**
 * A bidirectional communication channel backed by Y.Array.
 *
 * This class allows multiple parties to send and receive messages through a shared
 * Y.Array CRDT.
 */
export class YjsChannel<T = unknown> {
  private readonly senderId: string
  private readonly array: Y.Array<ChannelMessage<T>>
  private readonly handlers: Set<MessageHandler<T>> = new Set()
  private readonly observeHandler: (event: Y.YArrayEvent<ChannelMessage<T>>) => void

  /**
   * Creates a new YjsChannel.
   * @param doc - The shared Y.Doc document
   * @param channelName - The name of the channel (used to get/create the Y.Array)
   */
  constructor(doc: Y.Doc, channelName: string) {
    this.senderId = crypto.randomUUID()
    this.array = doc.getArray<ChannelMessage<T>>(channelName)

    this.observeHandler = (event: Y.YArrayEvent<ChannelMessage<T>>) => {
      // Process all added items
      for (const delta of event.changes.delta) {
        if (delta.insert) {
          const items = Array.isArray(delta.insert) ? delta.insert : [delta.insert]
          for (const item of items) {
            // Only notify handlers if the message is from another sender
            if (item.senderId !== this.senderId) {
              this.notifyHandlers(item.payload)
            }
          }
        }
      }
    }

    this.array.observe(this.observeHandler)
  }

  /**
   * Sends a message to the channel.
   * @param message - The message to send
   */
  send(message: T): void {
    const channelMessage: ChannelMessage<T> = {
      senderId: this.senderId,
      payload: message,
    }
    this.array.push([channelMessage])
  }

  /**
   * Subscribes to messages received from other parties.
   * @param handler - The callback to invoke when a message is received
   * @returns A function to unsubscribe the handler
   */
  subscribe(handler: MessageHandler<T>): () => void {
    this.handlers.add(handler)
    return () => {
      this.handlers.delete(handler)
    }
  }

  /**
   * Removes all message handlers and stops observing the Y.Array.
   */
  dispose(): void {
    this.array.unobserve(this.observeHandler)
    this.handlers.clear()
  }

  /**
   * Notifies all subscribed handlers with the received message.
   */
  private notifyHandlers(message: T): void {
    for (const handler of this.handlers) {
      handler(message)
    }
  }
}
