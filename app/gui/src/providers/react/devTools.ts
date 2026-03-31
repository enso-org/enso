import * as react from 'react'
import { toRef } from 'vue'
import type { EnsoDevtoolsStore } from '../devTools'
import { useInReactFunction, useVueRef, useVueValue } from './common'

export const EnsoDevtoolsStoreContext = react.createContext<EnsoDevtoolsStore | null>(null)
export const useEnsoDevtoolsStore = useInReactFunction(EnsoDevtoolsStoreContext)

/** Whether the version checker is forcibly shown/hidden. */
export function useShowVersionChecker() {
  const store = useEnsoDevtoolsStore()
  return useVueRef(react.useCallback(() => toRef(store, 'showVersionChecker'), [store]))
}

/** A hook that provides access to the paywall devtools. */
export function usePaywallDevtools() {
  const store = useEnsoDevtoolsStore()
  const features = useVueValue(react.useCallback(() => store.paywallFeatures, [store]))
  return { features, setFeature: store.setPaywallFeature }
}

/** Whether Enso Dev tools are shown. */
export function useShowEnsoDevtools() {
  const store = useEnsoDevtoolsStore()
  return useVueRef(react.useCallback(() => toRef(store, 'showEnsoDevtools'), [store]))
}

/** Whether the version checker is forcibly shown/hidden. */
export function useShowDevtools() {
  const store = useEnsoDevtoolsStore()
  return useVueRef(react.useCallback(() => toRef(store, 'showDevtools'), [store]))
}
