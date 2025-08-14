/**
 * @file
 *
 * Re-exporte zustand functions and types.
 * Overrides the default `useStore` with a custom one that supports equality functions and `React.transition`.
 */
import { LOGOUT_EVENT } from '$/providers/session/constants'
import type { Mutate, StoreApi, StoreMutatorIdentifier } from 'zustand'
export {
  useStore,
  type AreEqual,
  type EqualityFunction,
  type EqualityFunctionName,
  type UseStoreOptions,
} from '#/hooks/storeHooks'
export { createStore, type StoreApi } from 'zustand'

/** Make a store reset on logout. */
export function resetStoreOnLogout<T, Mos extends [StoreMutatorIdentifier, unknown][] = []>(
  store: Mutate<StoreApi<T>, Mos>,
) {
  document.addEventListener(LOGOUT_EVENT, () => {
    store.setState(store.getInitialState())
  })
}
