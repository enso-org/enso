/**
 * @file
 *
 * Re-exporting zustand functions and types.
 * Overrides the default `useStore` with a custom one, that supports equality functions and React.transition
 */
export { useStore, useTearingTransitionStore } from '#/hooks/storeHooks'
export type {
  AreEqual,
  EqualityFunction,
  EqualityFunctionName,
  UseStoreOptions,
} from '#/hooks/storeHooks'
export { createStore, type StoreApi } from 'zustand'
