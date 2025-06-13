/**
 * @file
 *
 * Re-exporting zustand functions and types.
 * Overrides the default `useStore` with a custom one, that supports equality functions and React.transition
 */
export {
  useStore,
  useTearingTransitionStore,
  type AreEqual,
  type EqualityFunction,
  type EqualityFunctionName,
  type UseStoreOptions,
} from '#/hooks/storeHooks'
export { createStore, type StoreApi } from 'zustand'
