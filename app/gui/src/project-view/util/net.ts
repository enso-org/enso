import { onScopeDispose } from 'vue'
import { AbortScope } from 'ydoc-shared/util/net'
import {
  ReconnectingWebSocket,
  ReconnectingWebSocketTransport,
} from 'ydoc-shared/util/net/ReconnectingWSTransport'

export { AbortScope }

const WS_OPTIONS = {
  // We do not want to enqueue any messages, because after reconnecting we have to initProtocol again.
  maxEnqueuedMessages: 0,
}

/** TODO: Add docs */
export function createRpcTransport(url: string): ReconnectingWebSocketTransport {
  return new ReconnectingWebSocketTransport(url, WS_OPTIONS)
}

/** TODO: Add docs */
export function createDataWebsocket(url: string, binaryType: 'arraybuffer' | 'blob'): WebSocket {
  const websocket = new ReconnectingWebSocket(url, undefined, WS_OPTIONS)
  websocket.binaryType = binaryType
  return websocket as WebSocket
}

export interface WebSocketHandler {
  (
    data: string | ArrayBufferLike | Blob | ArrayBufferView,
    send: (data: string | ArrayBufferLike | Blob | ArrayBufferView) => void,
  ): void
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
