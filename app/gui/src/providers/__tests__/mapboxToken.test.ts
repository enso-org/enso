import { ConditionVariable } from '$/utils/ConditionVariable'
import { withSetup } from '@/util/testing'
import type { MapboxToken } from 'enso-common/src/services/Backend'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { effectScope } from 'vue'
import { createMapboxTokenStore } from '../mapboxToken'

const MOCK_EXPIRE_TIME = 60 * 60 * 1000

describe('Mapbox token', () => {
  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  test('is same ref when getting multiple times', () =>
    withSetup(async () => {
      const token = '1234'
      const getTokenFunction = vi.fn(() => {
        return Promise.resolve({
          token,
          expires: new Date(Date.now() + MOCK_EXPIRE_TIME),
        } satisfies MapboxToken)
      })
      const mapboxTokenStore = createMapboxTokenStore({
        getMapboxToken: getTokenFunction,
      })
      // Keep the scope after await.
      const scope = effectScope()
      const ref1 = await mapboxTokenStore.acquire()
      const ref2 = await scope.run(() => mapboxTokenStore.acquire())!
      expect(getTokenFunction).toHaveBeenCalledOnce()
      // toBe, because the Ref should be the same
      expect(ref1.value).toBe(ref2.value)
      expect(ref1.value.token).toBe(token)
    }))

  test('is same ref when getting multiple times before backend response', () =>
    withSetup(async () => {
      const token = '1234'
      let returnToken = false
      const returnTokenCondVar = new ConditionVariable()
      const getTokenFunction = vi.fn(async () => {
        while (!returnToken) await returnTokenCondVar.wait()
        return {
          token,
          expires: new Date(Date.now() + MOCK_EXPIRE_TIME),
        } satisfies MapboxToken
      })
      const mapboxTokenStore = createMapboxTokenStore({
        getMapboxToken: getTokenFunction,
      })
      const ref1Promise = mapboxTokenStore.acquire()
      const ref2Promise = mapboxTokenStore.acquire()
      returnToken = true
      returnTokenCondVar.notifyAll()
      const ref1 = await ref1Promise
      const ref2 = await ref2Promise
      expect(getTokenFunction).toHaveBeenCalledOnce()
      // toBe, because the Ref should be the same
      expect(ref1.value).toBe(ref2.value)
      expect(ref1.value.token).toBe(token)
    }))

  test('is refetched', () =>
    withSetup(async () => {
      const tokens = ['1234', '5678']
      const tokensQueue = [...tokens]
      const getTokenFunction = vi.fn(() => {
        return Promise.resolve({
          token: tokensQueue.shift() ?? 'NO-TOKEN',
          expires: new Date(Date.now() + MOCK_EXPIRE_TIME),
        } satisfies MapboxToken)
      })
      const mapboxTokenStore = createMapboxTokenStore({
        getMapboxToken: getTokenFunction,
      })
      const ref = await mapboxTokenStore.acquire()
      expect(ref.value.token).toBe(tokens[0])
      await vi.advanceTimersToNextTimerAsync()
      expect(ref.value.token).toBe(tokens[1])
      expect(getTokenFunction).toHaveBeenCalledTimes(tokens.length)
    }))

  test('is not refetched if all refs are disposed', () =>
    withSetup(async () => {
      const token = '1234'
      const getTokenFunction = vi.fn(() => {
        return Promise.resolve({
          token,
          expires: new Date(Date.now() + MOCK_EXPIRE_TIME),
        } satisfies MapboxToken)
      })
      const mapboxTokenStore = createMapboxTokenStore({
        getMapboxToken: getTokenFunction,
      })
      const scope = effectScope()
      await scope.run(async () => {
        const ref = await mapboxTokenStore.acquire()
        expect(ref.value.token).toBe(token)
      })
      scope.stop()
      await vi.runAllTimersAsync()
      // After timer run, we still requested for token only once
      expect(getTokenFunction).toHaveBeenCalledOnce()
    }))
})
