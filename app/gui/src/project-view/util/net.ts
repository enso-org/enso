import { onScopeDispose } from 'vue'
import {
  AbortScope,
  MockWebSocketTransport,
  ReconnectingWebSocket,
  ReconnectingWebSocketTransport,
} from 'ydoc-shared/util/net'

export { AbortScope, MockWebSocketTransport }

const WS_OPTIONS = {
  // We do not want to enqueue any messages, because after reconnecting we have to initProtocol again.
  maxEnqueuedMessages: 0,
}

/** TODO: Add docs */
export function createRpcTransport(url: string): ReconnectingWebSocketTransport {
  if (url.startsWith('mock://')) {
    const mockName = url.slice('mock://'.length)
    return new MockWebSocketTransport(mockName)
  } else {
    const transport = new ReconnectingWebSocketTransport(url, WS_OPTIONS)
    return transport
  }
}

/** TODO: Add docs */
export function createDataWebsocket(url: string, binaryType: 'arraybuffer' | 'blob'): WebSocket {
  if (url.startsWith('mock://')) {
    const mockWs = new MockWebSocket(url, url.slice('mock://'.length))
    mockWs.binaryType = binaryType
    return mockWs
  } else {
    const websocket = new ReconnectingWebSocket(url, undefined, WS_OPTIONS)
    websocket.binaryType = binaryType
    return websocket as WebSocket
  }
}

export interface WebSocketHandler {
  (
    data: string | ArrayBufferLike | Blob | ArrayBufferView,
    send: (data: string | ArrayBufferLike | Blob | ArrayBufferView) => void,
  ): void
}

/** TODO: Add docs */
export class MockWebSocket extends EventTarget implements WebSocket {
  static mocks: Map<string, WebSocketHandler> = new Map()
  readonly CONNECTING = WebSocket.CONNECTING
  readonly OPEN = WebSocket.OPEN
  readonly CLOSING = WebSocket.CLOSING
  readonly CLOSED = WebSocket.CLOSED
  readyState: number = WebSocket.OPEN
  binaryType: BinaryType = 'blob'
  readonly bufferedAmount = 0
  readonly extensions = ''
  readonly protocol = ''
  onopen: ((this: WebSocket, ev: Event) => any) | null = null
  onclose: ((this: WebSocket, ev: CloseEvent) => any) | null = null
  onmessage: ((this: WebSocket, ev: MessageEvent<any>) => any) | null = null
  onerror: ((this: WebSocket, ev: Event) => any) | null = null

  /** TODO: Add docs */
  static addMock(name: string, data: WebSocketHandler) {
    MockWebSocket.mocks.set(name, data)
  }

  /** TODO: Add docs */
  constructor(
    public url: string,
    public name: string,
  ) {
    super()
    this.addEventListener('open', (ev) => this.onopen?.(ev))
    this.addEventListener('close', (ev) => this.onclose?.(ev as CloseEvent))
    // deepcode ignore InsufficientPostmessageValidation: This is not a `postMessage`.
    this.addEventListener('message', (ev) => this.onmessage?.(ev as MessageEvent<any>))
    this.addEventListener('error', (ev) => this.onerror?.(ev))
    setTimeout(() => this.dispatchEvent(new Event('open')), 0)
  }

  /** TODO: Add docs */
  send(data: string | ArrayBufferLike | Blob | ArrayBufferView): void {
    MockWebSocket.mocks.get(this.name)?.(data, (data) =>
      this.dispatchEvent(new MessageEvent('message', { data })),
    )
  }
  /** TODO: Add docs */
  close() {
    this.readyState = WebSocket.CLOSED
  }
}

type QueueTask<State> = (state: State) => Promise<State>

/**
 * A serializing queue of asynchronous tasks transforming a state. Each task is a function that
 * takes the current state and produces a promise to the transformed state. Each task waits for the
 * previous task to finish before starting.
 */
export class AsyncQueue<State> {
  lastTask: Promise<State>
  taskRunning = false
  queuedTasks: QueueTask<State>[] = []

  /** TODO: Add docs */
  constructor(initTask: Promise<State>) {
    this.lastTask = initTask
  }

  private run() {
    if (this.taskRunning) return
    const task = this.queuedTasks.shift()
    if (task == null) return
    this.taskRunning = true
    this.lastTask = this.lastTask
      .then(
        (state) => task(state),
        (error) => {
          console.error(
            "AsyncQueue failed to run task '" + task.toString() + "' with error:",
            error,
          )
          throw error
        },
      )
      .finally(() => {
        this.taskRunning = false
        this.run()
      })
  }

  /** TODO: Add docs */
  pushTask(f: QueueTask<State>) {
    this.queuedTasks.push(f)
    this.run()
  }

  /** TODO: Add docs */
  clear() {
    this.queuedTasks.length = 0
  }

  /** TODO: Add docs */
  async waitForCompletion(): Promise<State> {
    let lastState: State
    do {
      lastState = await this.lastTask
    } while (this.taskRunning)
    return lastState
  }
}

/** Create an abort signal that is signalled when containing Vue scope is disposed. */
export function useAbortScope(): AbortScope {
  const scope = new AbortScope()
  onScopeDispose(() => scope.dispose('Vue scope disposed.'))
  return scope
}
