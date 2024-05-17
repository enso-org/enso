import { Vec2 } from '@/util/data/vec2'
import { debouncedWatch, useLocalStorage } from '@vueuse/core'
import { encoding } from 'lib0'
import { xxHash128 } from 'shared/ast/ffi'
import { computed, nextTick, watch } from 'vue'
import type { NavigatorComposable } from './navigator'

/**
 * Synchronize given navigator's viewport pan and zoom with `localStorage`.
 *
 * @param navigator The navigator representing a viewport to synchronize.
 * @param reactiveStorageKeyEncoder A **reactive** encoder from which a storage key is derived. Data
 * that is encoded in this function dictates the effective identity of stored viewport. Whenever the
 * encoded data changes, the stored viewport value is restored to navigator.
 * @param initializeViewport A function that will be called when no stored viewport is found for the
 * current storage key.
 */
export function useNavigatorStorage(
  navigator: NavigatorComposable,
  reactiveStorageKeyEncoder: (enc: encoding.Encoder) => void,
  initializeViewport: () => void,
) {
  const graphViewportStorageKey = computed(() =>
    xxHash128(encoding.encode(reactiveStorageKeyEncoder)),
  )

  type ViewportStorage = Map<string, { x: number; y: number; s: number }>
  const storedViewport = useLocalStorage<ViewportStorage>('enso-viewport', new Map())
  /**
   * Maximum number of viewports stored in localStorage. When it is exceeded, least recently used
   * half of the stored data is removed.
   */
  const MAX_STORED_VIEWPORTS = 256

  // Save/Load viewports whenever entering a new graph context (i.e. the storage key has changed).
  watch(
    graphViewportStorageKey,
    (key, prevKey) => {
      if (key === prevKey) return
      if (prevKey != null) storeCurrentViewport(prevKey)
      restoreViewport(key)
    },
    { immediate: true },
  )

  // Whenever the viewport was changed and stable for a while, save it in localstorage.
  debouncedWatch(
    () => [navigator.targetCenter, navigator.targetScale],
    () => storeCurrentViewport(graphViewportStorageKey.value),
    { debounce: 200 },
  )

  function storeCurrentViewport(storageKey: string) {
    const pos = navigator.targetCenter
    const scale = navigator.targetScale
    storedViewport.value.set(storageKey, { x: pos.x, y: pos.y, s: scale })
    // Ensure that the storage doesn't grow forever by periodically removing least recently
    // written half of entries when we reach a limit.
    if (storedViewport.value.size > MAX_STORED_VIEWPORTS) {
      let toRemove = storedViewport.value.size - MAX_STORED_VIEWPORTS / 2
      for (const key of storedViewport.value.keys()) {
        if (toRemove-- <= 0) break
        storedViewport.value.delete(key)
      }
    }
  }

  function restoreViewport(storageKey: string) {
    const restored = storedViewport.value.get(storageKey)
    if (restored == null) nextTick(initializeViewport)
    const pos = restored ? Vec2.FromXY(restored).finiteOrZero() : Vec2.Zero
    const scale = restored?.s ?? 1
    navigator.setCenterAndScale(pos, scale)
  }
}
