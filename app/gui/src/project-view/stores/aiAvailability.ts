/** @file Cached "is the local Claude agent available?" probe shared across the ProjectView UI. */

import { proxyRefs } from '$/utils/reactivity'
import { createGlobalState } from '@vueuse/core'
import { ref } from 'vue'

function createAiAvailabilityStore() {
  const availability = ref(false)
  const loaded = ref(false)

  const electronApi = typeof window === 'undefined' ? undefined : window.api
  const promise = electronApi != null ? electronApi.ai.isAvailable() : Promise.resolve(false)

  promise.then((result) => (availability.value = result)).finally(() => (loaded.value = true))

  return proxyRefs({
    availability,
    loaded,
    promise,
  })
}

/**
 * Reactive availability of the local Claude agent: `true` once the Electron main process has
 * reported that `claude` spawned without a synchronous error (cheap, decoupled from priming).
 */
export const useAiAvailability = createGlobalState(createAiAvailabilityStore)
