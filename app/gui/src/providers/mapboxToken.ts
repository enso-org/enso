import { useBackends } from '$/providers/backends'
import { clamp } from '$/utils/data/math'
import { createGlobalState } from '@vueuse/core'
import type { MapboxToken } from 'enso-common/src/services/Backend'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { onScopeDispose, ref, type Ref } from 'vue'

/**
 * Minimum sensible time to refetch mapbox token.
 *
 * If the time between refetches would be lower, we assume user has wrong system time, and this avoid
 * a potential flood of token requests.
 */
const MIN_TIME_TO_REFETCH_MS = 60 * 1000
/** Maximum sensible time to refetch mapbox token. See {@link MIN_TIME_TO_REFETCH_MS} for details. */
const MAX_TIME_TO_REFETCH_MS = 2 ** 32 - 1

/** Constructor of Mapbox Token Store - see {@link useMapboxToken} */
export function createMapboxTokenStore(remoteBackend: Pick<RemoteBackend, 'getMapboxToken'>) {
  let mapboxToken: Promise<Ref<MapboxToken>> | undefined
  let refCount = 0

  function fetchToken() {
    const promise = remoteBackend.getMapboxToken()
    // Schedule token refetch
    promise.then((token) => {
      let time = token.expires.getTime() - Date.now()
      if (time < MIN_TIME_TO_REFETCH_MS || time > MAX_TIME_TO_REFETCH_MS) {
        console.warn(
          `Time to refetch Mapbox token has unreasonable value ${time}. Probably system time is wrong.`,
        )
        time = clamp(time, MIN_TIME_TO_REFETCH_MS, MAX_TIME_TO_REFETCH_MS)
        console.warn(`Set time to ${time} instead.`)
      }
      setTimeout(() => {
        if (refCount > 0) fetchToken()
        else mapboxToken = undefined
      }, time)
    })

    if (mapboxToken) {
      Promise.all([promise, mapboxToken]).then(([token, ref]) => (ref.value = token))
    } else {
      mapboxToken = promise.then((token) => ref(token))
    }
    return mapboxToken
  }

  function acquire() {
    refCount += 1
    onScopeDispose(() => {
      refCount -= 1
    })
    if (mapboxToken != null) return mapboxToken
    else return fetchToken()
  }

  return {
    acquire,
  }
}

/**
 * Mapbox Token Store.
 *
 * It provides `acquire` method returning ref with token. The ref will be updated when token
 * expires. `acquire` must be called in vue's EffectScope.
 */
export const useMapboxToken = createGlobalState(() => {
  const { remoteBackend } = useBackends()
  return createMapboxTokenStore(remoteBackend)
})
