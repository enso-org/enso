import { Err, Error, Ok, rejectionToResult, type Result } from '@/util/result'
import { wait } from 'lib0/promise'
import { LsRpcError } from 'shared/languageServer'

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

  async waitForCompletion(): Promise<State> {
    let lastState: State
    do {
      console.log('this.lastTask', this.lastTask)
      lastState = await this.lastTask
      console.log('lastState', lastState)
    } while (this.taskRunning)
    return lastState
  }
}

if (import.meta.vitest) {
  const { describe, test, expect, beforeEach, afterEach, vi } = import.meta.vitest

  beforeEach(() => {
    vi.useFakeTimers()
  })
  afterEach(() => {
    vi.useRealTimers()
  })

  describe('AsyncQueue', () => {
    test('sets initial state', async () => {
      const queue = new AsyncQueue(Promise.resolve(1))
      expect(await queue.waitForCompletion()).toBe(1)
    })

    test('runs tasks in sequence', async () => {
      const queue = new AsyncQueue(Promise.resolve(1))
      queue.pushTask(async (state) => {
        expect(state).toBe(1)
        await wait(100)
        return 2
      })
      queue.pushTask(async (state) => {
        expect(state).toBe(2)
        return 3
      })
      vi.runAllTimersAsync()
      expect(await queue.waitForCompletion()).toBe(3)
    })

    test('clear removes all not yet started tasks', async () => {
      const queue = new AsyncQueue(Promise.resolve(1))
      queue.pushTask(async (state) => {
        expect(state).toBe(1)
        await wait(100)
        return 2
      })
      queue.pushTask(async (state) => {
        expect(state).toBe(2)
        return 3
      })
      queue.clear()
      queue.pushTask(async (state) => {
        expect(state).toBe(2)
        return 5
      })
      vi.runAllTimersAsync()
      expect(await queue.waitForCompletion()).toBe(5)
    })
  })

  describe('exponentialBackoff', () => {
    test('runs successful task once', async () => {
      const task = vi.fn(async () => Ok(1))
      const result = await exponentialBackoff(task)
      expect(result).toEqual({ ok: true, value: 1 })
      expect(task).toHaveBeenCalledTimes(1)
    })

    test('retry failing task up to a limit', async () => {
      const task = vi.fn(async () => Err(1))
      const promise = exponentialBackoff(task, { maxRetries: 4 })
      vi.runAllTimersAsync()
      const result = await promise
      expect(result).toEqual({ ok: false, error: new Error(1) })
      expect(task).toHaveBeenCalledTimes(5)
    })

    test('wait before retrying', async () => {
      const task = vi.fn(async () => Err(null))
      exponentialBackoff(task, {
        maxRetries: 10,
        retryDelay: 100,
        retryDelayMultiplier: 3,
        retryDelayMax: 1000,
      })
      expect(task).toHaveBeenCalledTimes(1)
      await vi.advanceTimersByTimeAsync(100)
      expect(task).toHaveBeenCalledTimes(2)
      await vi.advanceTimersByTimeAsync(300)
      expect(task).toHaveBeenCalledTimes(3)
      await vi.advanceTimersByTimeAsync(900)
      expect(task).toHaveBeenCalledTimes(4)
      await vi.advanceTimersByTimeAsync(5000)
      expect(task).toHaveBeenCalledTimes(9)
    })

    test('retry task until success', async () => {
      const task = vi.fn()
      task.mockReturnValueOnce(Promise.resolve(Err(3)))
      task.mockReturnValueOnce(Promise.resolve(Err(2)))
      task.mockReturnValueOnce(Promise.resolve(Ok(1)))
      const promise = exponentialBackoff(task)
      vi.runAllTimersAsync()
      const result = await promise
      expect(result).toEqual({ ok: true, value: 1 })
      expect(task).toHaveBeenCalledTimes(3)
    })

    test('call retry callback', async () => {
      const task = vi.fn()
      task.mockReturnValueOnce(Promise.resolve(Err(3)))
      task.mockReturnValueOnce(Promise.resolve(Err(2)))
      task.mockReturnValueOnce(Promise.resolve(Ok(1)))
      const onBeforeRetry = vi.fn()

      const promise = exponentialBackoff(task, { onBeforeRetry })
      vi.runAllTimersAsync()
      await promise
      expect(onBeforeRetry).toHaveBeenCalledTimes(2)
      expect(onBeforeRetry).toHaveBeenNthCalledWith(1, new Error(3), 0, 1000)
      expect(onBeforeRetry).toHaveBeenNthCalledWith(2, new Error(2), 1, 2000)
    })
  })
}
