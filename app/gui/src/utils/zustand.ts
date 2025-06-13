import { defaultEquality } from '@/util/equals'
import { DeepReadonly, onScopeDispose, readonly, Ref, shallowRef } from 'vue'
import { StoreApi } from 'zustand'

/**
 * Get zustand store as Vue ref.
 *
 * The ref will be updated only when the value changed according to eq function
 */
export function useZustantStoreRef<State, Slice>(
  store: StoreApi<State>,
  getter: (state: State) => Slice,
  eq: (a: Slice, b: Slice) => boolean = defaultEquality,
): DeepReadonly<Ref<Slice>> {
  const value = shallowRef(getter(store.getState()))

  const unsubscribe = store.subscribe((state) => {
    const newVal = getter(state)
    if (!eq(value.value, newVal)) {
      value.value = newVal
    }
  })
  onScopeDispose(unsubscribe)

  return readonly(value)
}
