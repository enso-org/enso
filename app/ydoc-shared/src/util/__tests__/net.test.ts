import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { Err, Ok, ResultError } from '../data/result'
import { exponentialBackoff } from '../net'

beforeEach(() => {
  vi.useFakeTimers()
})
afterEach(() => {
  vi.useRealTimers()
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
    expect(onBeforeRetry).toHaveBeenNthCalledWith(1, new ResultError(3), 0, 3, 1000)
    expect(onBeforeRetry).toHaveBeenNthCalledWith(2, new ResultError(2), 1, 3, 2000)
  })
})
