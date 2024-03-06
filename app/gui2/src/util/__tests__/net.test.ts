import { Err, Ok, ResultError } from '@/util/data/result'
import { AsyncQueue, exponentialBackoff } from '@/util/net'
import { wait } from 'lib0/promise'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'

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
    expect(result).toEqual({ ok: false, error: new ResultError(1) })
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
    expect(onBeforeRetry).toHaveBeenNthCalledWith(1, new ResultError(3), 0, 1000)
    expect(onBeforeRetry).toHaveBeenNthCalledWith(2, new ResultError(2), 1, 2000)
  })
})
