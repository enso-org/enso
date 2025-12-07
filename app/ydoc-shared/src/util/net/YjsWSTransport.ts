/**
 * A JSON-RPC transport implementation that uses YjsChannel for communication
 * instead of WebSocket. This allows JSON-RPC to work over Y.js CRDT
 * synchronization.
 */

import { JSONRPCError } from '@open-rpc/client-js'
import { ERR_UNKNOWN } from '@open-rpc/client-js/build/Error.js'
import {
  getBatchRequests,
  getNotifications,
  type JSONRPCRequestData,
} from '@open-rpc/client-js/build/Request.js'
import { Transport } from '@open-rpc/client-js/build/transports/Transport.js'
import { YjsChannel } from 'ydoc-channel'
import type * as Y from 'yjs'

export interface AddEventListenerOptions {
  capture?: boolean
  once?: boolean
  passive?: boolean
  signal?: AbortSignal
}

/** Event map for YjsWSTransport events. */
interface YjsWSEventMap {
  open: Event
  close: CloseEvent
  message: MessageEvent<string>
  error: ErrorEvent
}

type EventListener<T = Event> = (event: T) => void

/** A JSON-RPC transport that uses YjsChannel for communication. */
export class YjsWSTransport extends Transport {
  private channel: YjsChannel<string>
  private yjsUnsubscribe?: (() => void) | undefined
  private eventListeners: Map<string, Set<EventListener<any>>> = new Map()

  /**
   * Create a {@link YjsWSTransport}.
   * @param doc - The shared Y.Doc document
   * @param channelName - The name of the channel (used to get/create the Y.Array)
   */
  constructor(doc: Y.Doc, channelName: string) {
    super()
    this.channel = new YjsChannel<string>(doc, channelName)
  }

  /**
   * Initiate the channel subscription.
   */
  public connect(): Promise<void> {
    return new Promise((resolve) => {
      this.yjsUnsubscribe = this.channel.subscribe((message) => {
        this.emit('message', new MessageEvent('message', { data: message }))
        this.transportRequestManager.resolveResponse(message)
      })
      this.emit('open', new Event('open'))
      resolve()
    })
  }

  /**
   * Send JSON-RPC data through the channel.
   */
  public async sendData(data: JSONRPCRequestData, timeout: number | null = 5000): Promise<any> {
    let prom = this.transportRequestManager.addRequest(data, timeout)
    const notifications = getNotifications(data)
    try {
      this.channel.send(JSON.stringify(this.parseData(data)))
      this.transportRequestManager.settlePendingRequest(notifications)
    } catch (err) {
      const jsonError = new JSONRPCError((err as any).message, ERR_UNKNOWN, err)

      this.emit('error', new ErrorEvent('error', { error: err, message: (err as any).message }))
      this.transportRequestManager.settlePendingRequest(notifications, jsonError)
      this.transportRequestManager.settlePendingRequest(getBatchRequests(data), jsonError)

      prom = Promise.reject(jsonError)
    }

    return prom
  }

  /** Close the channel and clean up subscriptions. */
  public close(): void {
    if (this.yjsUnsubscribe) {
      this.yjsUnsubscribe()
      this.yjsUnsubscribe = undefined
    }
    this.channel.dispose()
    this.emit('close', new CloseEvent('close'))
  }

  /** Add an event listener. */
  on<K extends keyof YjsWSEventMap>(
    type: K,
    cb: (event: YjsWSEventMap[K]) => void,
    options?: AddEventListenerOptions,
  ): void {
    if (!this.eventListeners.has(type)) {
      this.eventListeners.set(type, new Set())
    }

    const wrappedCb = (event: YjsWSEventMap[K]) => {
      cb(event)
      if (options?.once) {
        this.off(type, cb)
      }
    }

    // Store original callback for later removal
    (wrappedCb as any).__original = cb

    this.eventListeners.get(type)!.add(wrappedCb)

    // Handle abort signal
    if (options?.signal) {
      options.signal.addEventListener('abort', () => {
        this.off(type, cb)
      })
    }
  }

  /** Remove an event listener. */
  off<K extends keyof YjsWSEventMap>(
    type: K,
    cb: (event: YjsWSEventMap[K]) => void,
  ): void {
    const listeners = this.eventListeners.get(type)
    if (!listeners) return

    // Find and remove the listener with matching original callback
    for (const listener of listeners) {
      if ((listener as any).__original === cb || listener === cb) {
        listeners.delete(listener)
        break
      }
    }
  }

  /** Emit an event to all registered listeners. */
  private emit<K extends keyof YjsWSEventMap>(type: K, event: YjsWSEventMap[K]): void {
    const listeners = this.eventListeners.get(type)
    if (!listeners) return

    for (const listener of listeners) {
      listener(event)
    }
  }
}
