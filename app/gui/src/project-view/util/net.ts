import { onScopeDispose } from 'vue'
import { YjsChannel } from 'ydoc-channel'
import { AbortScope } from 'ydoc-shared/util/net'
import { YjsTransport } from 'ydoc-shared/util/net/YjsTransport'
import * as Y from 'yjs'

export { AbortScope }

/** TODO: Add docs */
export function createRpcTransport(indexDoc: Y.Doc, url: string): YjsTransport {
  return new YjsTransport(indexDoc, url)
}

/** TODO: Add docs */
export function createDataSocket(indexDoc: Y.Doc, url: string): YjsChannel {
  return new YjsChannel(indexDoc, url)
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
