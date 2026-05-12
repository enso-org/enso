/** @file Cached "is the local Claude agent available?" probe shared across the ProjectView UI. */

import { createContextStore } from '@/providers'
import { ref, type Ref } from 'vue'

/**
 * Reactive availability probe for the local Claude agent. On construction it asks the Electron
 * main process whether `claude` spawned without a synchronous error (cheap, decoupled from
 * priming); the result lands in a single `Ref<boolean>` that consumers can read synchronously.
 *
 * In non-Electron builds (e.g. the browser-only build, or tests that mock `window.api`),
 * `window.api?.ai.isAvailable` may be absent — the ref stays at its initial `false`.
 */
export interface AiAvailabilityStore {
  readonly available: Readonly<Ref<boolean>>
}

function aiAvailabilityStoreFactory(): AiAvailabilityStore {
  const available = ref<boolean>(false)
  const electronApi = typeof window === 'undefined' ? undefined : window.api
  if (electronApi != null) {
    void electronApi.ai.isAvailable().then(
      (result) => {
        available.value = result
      },
      (err) => {
        console.warn('[AI] isAvailable probe failed; treating as unavailable.', err)
      },
    )
  }
  return { available }
}

export const [provideAiAvailability, useAiAvailability] = createContextStore(
  'aiAvailability',
  aiAvailabilityStoreFactory,
)
