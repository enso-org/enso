import { useAbortScope } from '@/util/net'
import { debouncedWatch, useLocalStorage } from '@vueuse/core'
import { encoding } from 'lib0'
import { computed, getCurrentInstance, ref, watch, withCtx } from 'vue'
import { xxHash128 } from 'ydoc-shared/ast/ffi'
import { AbortScope } from 'ydoc-shared/util/net'

export interface SyncLocalStorageOptions<StoredState> {
  /** The main localStorage key under which a map of saved states will be stored. */
  storageKey: string
  /** The minimum amount of time a serialized state is stable before it is written to localStorage. */
  debounce: number
  /**
   * A **reactive** key encoder used for distinguishing between separate stored map entries. Data
   * that is encoded in this function dictates the effective identity of stored state. Whenever the
   * encoded key changes, the current state is saved and state stored under new key is restored.
   */
  mapKeyEncoder: (enc: encoding.Encoder) => void
  /**
   * **Reactive** current state serializer. Captures the environment data that will be stored in
   * localStorage. Returned object must be JSON-encodable. State will not be captured while async
   * restore is still in progress.
   */
  captureState: () => StoredState
  /**
   * Stored state deserializer. Decodes previously stored state back to the environment. In case the
   * deserialization process is asynchronous, the `abort` signal must be respected - environment
   * should not be modified after abort has been signalled.
   *
   * The `state` is `undefined` when it hasn't been previously saved under current key.
   */
  restoreState: (
    state: Partial<StoredState> | undefined,
    abort: AbortSignal,
  ) => Promise<void> | void
}

/**
 * Synchronize local view state with `localStorage`. Supports saving and restoring multiple unique
 * states based on encoded identity key.
 */
export function useSyncLocalStorage<StoredState extends object>(
  options: SyncLocalStorageOptions<StoredState>,
) {
  const graphViewportStorageKey = computed(() => xxHash128(encoding.encode(options.mapKeyEncoder)))

  // Ensure that restoreState function is run within component's context, allowing for temporary
  // watchers to be created for async/await purposes.
  const restoreStateInCtx = withCtx(
    options.restoreState,
    getCurrentInstance(),
  ) as typeof options.restoreState

  const storageMap = useLocalStorage<Map<string, StoredState>>(options.storageKey, new Map())

  /**
   * Maximum number of graph states stored in localStorage. When it is exceeded, least recently used
   * half of the stored data is removed.
   */
  const MAX_STORED_GRAPH_STATES = 256

  const abortScope = useAbortScope()
  let restoreAbort: AbortScope | null
  function abortLastRestore() {
    if (restoreAbort) {
      restoreAbort.dispose('Restore aborted.')
      restoreAbort = null
    }
  }

  let nextRestoreId = 0
  const restoreIdInProgress = ref<number>()
  const serializedState = computed(() => options.captureState())

  // Save/Load viewports whenever entering a new graph context (i.e. the storage key has changed).
  watch(
    graphViewportStorageKey,
    (key, prevKey) => {
      if (key === prevKey) return
      if (prevKey != null && restoreIdInProgress.value == null) {
        saveState(prevKey, serializedState.value)
      }
      restoreState(key)
    },
    { immediate: true },
  )

  // Whenever the state was changed and stable for a while, save it in localStorage. Does not
  // perform saves when restore is still in progress.
  debouncedWatch(
    () => restoreIdInProgress.value == null && serializedState.value,
    () => {
      if (restoreIdInProgress.value == null) {
        saveState(graphViewportStorageKey.value, serializedState.value)
      }
    },
    {
      debounce: options.debounce,
    },
  )

  function saveState(storageKey: string, state: StoredState) {
    storageMap.value.set(storageKey, state)
    // Ensure that the storage doesn't grow forever by periodically removing least recently
    // written half of entries when we reach a limit.
    if (storageMap.value.size > MAX_STORED_GRAPH_STATES) {
      let toRemove = storageMap.value.size - MAX_STORED_GRAPH_STATES / 2
      for (const key of storageMap.value.keys()) {
        if (toRemove-- <= 0) break
        storageMap.value.delete(key)
      }
    }
  }

  async function restoreState(storageKey: string) {
    abortLastRestore()

    const thisRestoreId = nextRestoreId++
    restoreIdInProgress.value = thisRestoreId
    const restored = storageMap.value.get(storageKey)

    restoreAbort = abortScope.child()
    try {
      await restoreStateInCtx(restored, restoreAbort.signal)
    } catch (e) {
      // Ignore promise rejections caused by aborted scope. Those are expected to happen.
      if (!restoreAbort.signal.aborted) throw e
    } finally {
      if (restoreIdInProgress.value === thisRestoreId) restoreIdInProgress.value = undefined
    }
  }
}
