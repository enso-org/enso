/**
 * A JSON-RPC transport implementation that uses YjsChannel for communication.
 * This allows JSON-RPC to work over Y.js CRDT synchronization.
 */

import { ERR_UNKNOWN, JSONRPCError } from '@open-rpc/client-js/build/Error.js'
import {
  getBatchRequests,
  getNotifications,
  type JSONRPCRequestData,
} from '@open-rpc/client-js/build/Request.js'
import { Transport } from '@open-rpc/client-js/build/transports/Transport.js'
import type { YjsChannelServer } from 'ydoc-channel'
import { YjsChannel } from 'ydoc-channel'
import type * as Y from 'yjs'

export interface AddEventListenerOptions {
  capture?: boolean
  once?: boolean
  passive?: boolean
  signal?: AbortSignal
}

/** Event map for YjsTransport events. */
interface YjsEventMap {
  open: Event
  close: CloseEvent
  message: MessageEvent<string>
  error: ErrorEvent
}

type EventListener<T = Event> = (event: T) => void

/** A JSON-RPC transport that uses YjsChannel for communication. */
export class YjsTransport extends Transport {
  protected channel: YjsChannel<string>
  protected doc: Y.Doc
  protected channelName: string
  protected eventListeners: Map<string, Set<EventListener<any>>> = new Map()

  /**
   * Create a {@link YjsTransport}.
   * @param doc - The shared Y.Doc document
   * @param channelName - The name of the channel (used to get/create the Y.Array)
   */
  constructor(doc: Y.Doc, channelName: string) {
    super()
    this.doc = doc
    this.channelName = channelName
    this.channel = new YjsChannel<string>(doc, channelName)
  }

  /**
   * Initiate the channel subscription.
   */
  public connect(): Promise<void> {
    return new Promise((resolve) => {
      this.channel.subscribe((message) => {
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
      const message = JSON.stringify(this.parseData(data))
      this.channel.send(message)
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
    this.channel.close()
    this.emit('close', new CloseEvent('close'))
  }

  /** Add an event listener. */
  on<K extends keyof YjsEventMap>(
    type: K,
    cb: (event: YjsEventMap[K]) => void,
    options?: AddEventListenerOptions,
  ): void {
    if (!this.eventListeners.has(type)) {
      this.eventListeners.set(type, new Set())
    }

    const wrappedCb = (event: YjsEventMap[K]) => {
      cb(event)
      if (options?.once) {
        this.off(type, cb)
      }
    }

    // Store original callback for later removal
    ;(wrappedCb as any).__original = cb

    this.eventListeners.get(type)!.add(wrappedCb)

    // Handle abort signal
    if (options?.signal) {
      options.signal.addEventListener('abort', () => {
        this.off(type, cb)
      })
    }
  }

  /** Remove an event listener. */
  off<K extends keyof YjsEventMap>(type: K, cb: (event: YjsEventMap[K]) => void): void {
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
  protected emit<K extends keyof YjsEventMap>(type: K, event: YjsEventMap[K]): void {
    const listeners = this.eventListeners.get(type)
    if (!listeners) return

    for (const listener of listeners) {
      listener(event)
    }
  }
}

/** A JSON-RPC transport that uses YjsChannel for communication. */
export class YjsServerTransport extends YjsTransport {
  private readonly proxyChannel: YjsChannel
  private readonly server: YjsChannelServer

  /**
   * Create a {@link YjsTransport}.
   * @param doc - The shared Y.Doc document
   * @param channelName - The name of the channel (used to get/create the Y.Array)
   */
  constructor(doc: Y.Doc, channelName: string, server: YjsChannelServer) {
    super(doc, `backend-${channelName}`)
    this.server = server
    this.proxyChannel = new YjsChannel(doc, channelName)
  }

  /**
   * Initiate the channel subscription.
   */
  override connect(): Promise<void> {
    const proxyConnect = new Promise<void>((resolve) => {
      this.server.onConnect(this.proxyChannel)
      this.server.onConnect(new YjsChannel(this.doc, this.channelName))
      resolve()
    })
    return proxyConnect.then(() => super.connect())
  }

  /** Close the channel and clean up subscriptions. */
  override close(): void {
    this.proxyChannel.close()
    super.close()
  }
}
