/** @file Cached "is the local Claude agent available?" probe shared across the ProjectView UI. */

import { useFeatureFlag } from '$/providers/featureFlags'
import { proxyRefs } from '$/utils/reactivity'
import { createGlobalState } from '@vueuse/core'
import { computed, ref, watch, type Ref } from 'vue'

/** Dependencies of {@link aiAvailabilityStoreFactory}. */
export interface AiAvailabilityDeps {
  /** Whether the AI Component Browser mode is enabled at all (feature flag, off by default). */
  readonly enabled: Readonly<Ref<boolean>>
  /** Checks whether the local Claude agent can be spawned. */
  readonly probeAvailability: () => Promise<boolean>
}

/** Factory of the store behind {@link useAiAvailability}; exported for tests. */
export function aiAvailabilityStoreFactory({ enabled, probeAvailability }: AiAvailabilityDeps) {
  const probeResult = ref(false)
  const probeSettled = ref(false)

  let probe: Promise<boolean> | undefined
  // The probe spawns the `claude` executable, so it must not run while the feature is hidden
  // behind the flag.
  function startProbe(): Promise<boolean> {
    if (probe == null) {
      probe = probeAvailability()
      probe
        .then((result) => (probeResult.value = result))
        .finally(() => (probeSettled.value = true))
    }
    return probe
  }

  // With the feature hidden there is nothing to wait for; report "unavailable" right away so
  // startup does not block on a probe that will never run.
  const promise = enabled.value ? startProbe() : Promise.resolve(false)
  watch(enabled, (on) => {
    if (on) startProbe()
  })

  const availability = computed(() => enabled.value && probeResult.value)
  const loaded = computed(() => (enabled.value ? probeSettled.value : true))

  return proxyRefs({
    enabled: computed(() => enabled.value),
    availability,
    loaded,
    promise,
  })
}

/**
 * Reactive availability of the local Claude agent, gated by the `enableAiComponentBrowserMode`
 * feature flag: `true` once the flag is on and the Electron main process has reported that
 * `claude` spawned without a synchronous error (cheap, decoupled from priming).
 */
export const useAiAvailability = createGlobalState(() =>
  aiAvailabilityStoreFactory({
    enabled: useFeatureFlag('enableAiComponentBrowserMode'),
    probeAvailability: () => {
      const electronApi = typeof window === 'undefined' ? undefined : window.api
      return electronApi != null ? electronApi.ai.isAvailable() : Promise.resolve(false)
    },
  }),
)
