import { wait } from 'lib0/promise'
import { LsRpcError } from 'shared/languageServer'
import { rejectionToResult, type Error, type Result } from './result'

export interface BackoffOptions<E> {
  maxRetries?: number
  retryDelay?: number
  retryDelayMultiplier?: number
  retryDelayMax?: number
  /**
   * Called when the promise return an error result, and the next retry is about to be attempted.
   * When this function returns `false`, the backoff is immediately aborted. When this function is
   * not provided, the backoff will always continue until the maximum number of retries is reached.
   */
  onBeforeRetry?: (error: Error<E>, retryCount: number, delay: number) => boolean | void
}

const defaultBackoffOptions: Required<BackoffOptions<any>> = {
  maxRetries: 3,
  retryDelay: 1000,
  retryDelayMultiplier: 2,
  retryDelayMax: 10000,
  onBeforeRetry: () => {},
}

/**
 * Retry a failing promise function with exponential backoff.
 */
export async function exponentialBackoff<T, E>(
  f: () => Promise<Result<T, E>>,
  backoffOptions?: BackoffOptions<E>,
): Promise<Result<T, E>> {
  const options = { ...defaultBackoffOptions, ...backoffOptions }
  for (let retries = 0; ; retries += 1) {
    const result = await f()
    const delay = Math.min(
      options.retryDelayMax,
      options.retryDelay * options.retryDelayMultiplier ** retries,
    )
    if (
      result.ok ||
      retries >= options.maxRetries ||
      options.onBeforeRetry(result.error, retries, delay) === false
    ) {
      return result
    }
    await wait(delay)
  }
}

export const lsRequestResult = rejectionToResult(LsRpcError)

/**
 * Retry a failing Language Server RPC call with exponential backoff. The provided async function is
 * called on each retry.
 */
export async function rpcWithRetries<T>(
  f: () => Promise<T>,
  backoffOptions?: BackoffOptions<LsRpcError>,
): Promise<T> {
  const result = await exponentialBackoff(() => lsRequestResult(f()), backoffOptions)
  if (result.ok) return result.value
  else {
    console.error('Too many failed retries.')
    throw result.error
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

  constructor(initTask: Promise<State>) {
    this.lastTask = initTask
  }

  private run() {
    if (this.taskRunning) return
    const task = this.queuedTasks.shift()
    if (task == null) return
    this.taskRunning = true
    this.lastTask = this.lastTask
      .then((state) => task(state))
      .finally(() => {
        this.taskRunning = false
        this.run()
      })
  }

  pushTask(f: QueueTask<State>) {
    this.queuedTasks.push(f)
    this.run()
  }

  clear() {
    this.queuedTasks.length = 0
  }
}
