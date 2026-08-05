/** @file Unit tests for the feature-flag-gated availability probe of the local Claude agent. */

import { aiAvailabilityStoreFactory } from '@/stores/aiAvailability'
import { expect, test, vi } from 'vitest'
import { effectScope, nextTick, ref } from 'vue'

function setup(enabledInitially: boolean) {
  const enabled = ref(enabledInitially)
  const probeAvailability = vi.fn(async () => true)
  const scope = effectScope()
  const store = scope.run(() => aiAvailabilityStoreFactory({ enabled, probeAvailability }))!
  return { enabled, probeAvailability, scope, store }
}

test('stays unavailable and never probes while the flag is off', async () => {
  const { probeAvailability, scope, store } = setup(false)
  expect(store.enabled).toBe(false)
  expect(store.availability).toBe(false)
  expect(store.loaded).toBe(true)
  await expect(store.promise).resolves.toBe(false)
  expect(probeAvailability).not.toHaveBeenCalled()
  scope.stop()
})

test('probes and reports availability when the flag is on', async () => {
  const { probeAvailability, scope, store } = setup(true)
  expect(probeAvailability).toHaveBeenCalledTimes(1)
  await expect(store.promise).resolves.toBe(true)
  await nextTick()
  expect(store.enabled).toBe(true)
  expect(store.availability).toBe(true)
  expect(store.loaded).toBe(true)
  scope.stop()
})

test('probes lazily when the flag turns on, and hides again when it turns off', async () => {
  const { enabled, probeAvailability, scope, store } = setup(false)
  enabled.value = true
  await nextTick()
  expect(probeAvailability).toHaveBeenCalledTimes(1)
  await vi.waitFor(() => expect(store.availability).toBe(true))

  enabled.value = false
  await nextTick()
  expect(store.availability).toBe(false)

  enabled.value = true
  await nextTick()
  expect(probeAvailability).toHaveBeenCalledTimes(1)
  expect(store.availability).toBe(true)
  scope.stop()
})
