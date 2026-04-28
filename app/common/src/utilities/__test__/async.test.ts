import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { AsyncQueue } from '../async'

const wait = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms))

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
