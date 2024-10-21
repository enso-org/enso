import type {
  IJSONRPCNotificationResponse,
  JSONRPCRequestData,
} from '@open-rpc/client-js/build/Request'
import type { WebSocketEventMap } from 'partysocket/ws'
import { Notifications } from '../../languageServerTypes'
import { ReconnectingWebSocketTransport } from './ReconnectingWSTransport'

type ArgumentsType<T> = T extends (...args: infer U) => any ? U : never

export interface MockTransportData<Methods extends string = string> {
  (method: Methods, params: any, transport: MockWebSocketTransport): Promise<any>
}

/** A mock WebSocket transport, only for use in tests. */
export class MockWebSocketTransport extends ReconnectingWebSocketTransport {
  static mocks: Map<string, MockTransportData> = new Map()
  private openEventListeners = new Set<(event: WebSocketEventMap['open']) => void>()
  /** Create an {@link MockWebSocketTransport}. */
  constructor(public name: string) {
    super('')
  }

  /** Add a handler for the {@link MockWebSocketTransport} with the given name. */
  static addMock<Methods extends string>(name: string, data: MockTransportData<Methods>) {
    MockWebSocketTransport.mocks.set(name, data as any)
  }
  /** Simulate connecting to a WebSocket. */
  override connect(): Promise<any> {
    for (const listener of this.openEventListeners) listener(new Event('open'))
    return Promise.resolve()
  }
  /**
   * Simulate reconnecting to a WebSocket.
   * Currently unimplemented as the functionality is not needed for tests.
   */
  override reconnect() {}
  /**
   * Simulate closing a WebSocket.
   * Currently unimplemented as the functionality is not needed for tests.
   */
  override close(): void {}
  /**
   * Respond to the given JSON-RPC request, calling the mock implementation
   * registered with {@link MockWebSocketTransport['addMock']}.
   * Returns a rejected {@link Promise} if there is no corresponding mock implementation.
   */
  override sendData(data: JSONRPCRequestData, timeout?: number | null): Promise<any> {
    if (Array.isArray(data)) return Promise.all(data.map(d => this.sendData(d.request, timeout)))
    return (
      MockWebSocketTransport.mocks.get(this.name)?.(
        data.request.method,
        data.request.params,
        this,
      ) ?? Promise.reject()
    )
  }
  /** Emit a JSON-RPC notification. */
  emit<N extends keyof Notifications>(method: N, params: ArgumentsType<Notifications[N]>[0]): void {
    this.transportRequestManager.transportEventChannel.emit('notification', {
      jsonrpc: '2.0',
      method,
      params,
    } as IJSONRPCNotificationResponse)
  }

  /** Add an event listener for the given event. */
  override on<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
  ): void {
    if (type === 'open') this.openEventListeners.add(cb as any)
  }

  /** Remove an event listener for the given event. */
  override off<K extends keyof WebSocketEventMap>(
    type: K,
    cb: (
      event: WebSocketEventMap[K] extends Event ? WebSocketEventMap[K] : never,
    ) => WebSocketEventMap[K] extends Event ? void : never,
  ): void {
    if (type === 'open') this.openEventListeners.delete(cb as any)
  }
}

export interface WebSocketHandler {
  (
    data: string | ArrayBufferLike | Blob | ArrayBufferView,
    send: (data: string | ArrayBufferLike | Blob | ArrayBufferView) => void,
  ): void
}
