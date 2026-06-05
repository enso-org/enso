/** @file Utilities related to `async`. */

/**
 * A primitive that exposes a {@link Promise} alongside its resolve/reject functions, so
 * ownership of the promise's settlement can be passed around independently of where the
 * promise itself is awaited.
 */
export interface Deferred<T> {
  readonly promise: Promise<T>
  resolve: (value: T) => void
  reject: (reason: unknown) => void
}

/** Create a fresh {@link Deferred}. */
export function createDeferred<T>(): Deferred<T> {
  let resolve!: (value: T) => void
  let reject!: (reason: unknown) => void
  const promise = new Promise<T>((res, rej) => {
    resolve = res
    reject = rej
  })
  return { promise, resolve, reject }
}

/** A function to delay for a given number of milliseconds. */
export function delay(ms: number, abortSignal?: AbortSignal) {
  return new Promise((resolve, reject) => {
    const handle = setTimeout(resolve, ms)
    abortSignal?.addEventListener('abort', () => {
      clearTimeout(handle)
      reject(new Error('The `delay` timeout was aborted.'))
    })
  })
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
