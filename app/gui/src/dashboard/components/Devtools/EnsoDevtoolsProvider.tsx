/** @file A zustand store that contains the state of the Enso devtools. */
import { PAYWALL_FEATURES, type PaywallFeatureName } from '#/hooks/billing'
import * as zustand from '#/utilities/zustand'
import { unsafeEntries, unsafeFromEntries } from 'enso-common/src/utilities/data/object'
import { IS_DEV_MODE } from 'enso-common/src/utilities/detect'

/** Configuration for a paywall feature. */
export interface PaywallDevtoolsFeatureConfiguration {
  readonly isForceEnabled: boolean | null
}

/** The state of this zustand store. */
interface EnsoDevtoolsStore {
  readonly showDevtools: boolean
  readonly showEnsoDevtools: boolean
  readonly toggleEnsoDevtools: () => void
  readonly setShowDevtools: (showDevtools: boolean) => void
  readonly showVersionChecker: boolean | null
  readonly paywallFeatures: Record<PaywallFeatureName, PaywallDevtoolsFeatureConfiguration>
  readonly setPaywallFeature: (feature: PaywallFeatureName, isForceEnabled: boolean | null) => void
  readonly setEnableVersionChecker: (showVersionChecker: boolean | null) => void
}

export const ensoDevtoolsStore = zustand.createStore<EnsoDevtoolsStore>()((set) => ({
  showDevtools: IS_DEV_MODE,
  showEnsoDevtools: IS_DEV_MODE,
  toggleEnsoDevtools: () => {
    set(({ showEnsoDevtools }) => ({ showEnsoDevtools: !showEnsoDevtools }))
  },
  setShowDevtools: (showDevtools) => {
    set({ showDevtools, showEnsoDevtools: showDevtools })
  },
  showVersionChecker: false,
  paywallFeatures: unsafeFromEntries(
    unsafeEntries(PAYWALL_FEATURES).map(([feature]) => [feature, { isForceEnabled: null }]),
  ),
  setPaywallFeature: (feature, isForceEnabled) => {
    set((state) => ({
      paywallFeatures: { ...state.paywallFeatures, [feature]: { isForceEnabled } },
    }))
  },
  setEnableVersionChecker: (showVersionChecker) => {
    set({ showVersionChecker })
  },
}))

/** A function to set whether the version checker is forcibly shown/hidden. */
export function useEnableVersionChecker() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.showVersionChecker, {
    unsafeEnableTransition: true,
  })
}

/** A function to set whether the version checker is forcibly shown/hidden. */
export function useSetEnableVersionChecker() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.setEnableVersionChecker, {
    unsafeEnableTransition: true,
  })
}

/** A hook that provides access to the paywall devtools. */
export function usePaywallDevtools() {
  return zustand.useStore(
    ensoDevtoolsStore,
    (state) => ({ features: state.paywallFeatures, setFeature: state.setPaywallFeature }),
    { unsafeEnableTransition: true },
  )
}

/** A hook that provides access to the show enso devtools state. */
export function useShowEnsoDevtools() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.showEnsoDevtools, {
    unsafeEnableTransition: true,
  })
}

/** A hook that provides access to the show devtools state. */
export function useShowDevtools() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.showDevtools, {
    unsafeEnableTransition: true,
  })
}

/** A hook that provides access to the toggle enso devtools state. */
export function useToggleEnsoDevtools() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.toggleEnsoDevtools, {
    unsafeEnableTransition: true,
  })
}

/** A hook that provides access to the set show devtools state. */
export function useSetShowDevtools() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.setShowDevtools, {
    unsafeEnableTransition: true,
  })
}
