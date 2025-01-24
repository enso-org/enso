/**
 * @file
 * This file provides a zustand store that contains the state of the Enso devtools.
 */
import { PAYWALL_FEATURES, type PaywallFeatureName } from '#/hooks/billing'
import { unsafeEntries, unsafeFromEntries } from '#/utilities/object'
import * as zustand from '#/utilities/zustand'
import { IS_DEV_MODE } from 'enso-common/src/detect'
import { MotionGlobalConfig } from 'framer-motion'
import { persist } from 'zustand/middleware'

/** Configuration for a paywall feature. */
export interface PaywallDevtoolsFeatureConfiguration {
  readonly isForceEnabled: boolean | null
}

// =========================
// === EnsoDevtoolsStore ===
// =========================

/** The state of this zustand store. */
interface EnsoDevtoolsStore {
  readonly showDevtools: boolean | null
  readonly showEnsoDevtools: boolean | null
  readonly toggleEnsoDevtools: () => void
  readonly setShowDevtools: (showDevtools: boolean) => void
  readonly toggleDevtools: () => void
  readonly showVersionChecker: boolean | null
  readonly paywallFeatures: Record<PaywallFeatureName, PaywallDevtoolsFeatureConfiguration>
  readonly setPaywallFeature: (feature: PaywallFeatureName, isForceEnabled: boolean | null) => void
  readonly setEnableVersionChecker: (showVersionChecker: boolean | null) => void
  readonly animationsDisabled: boolean
  readonly setAnimationsDisabled: (animationsDisabled: boolean) => void
}

export const ensoDevtoolsStore = zustand.createStore<EnsoDevtoolsStore>()(
  persist(
    (set) => ({
      showDevtools: IS_DEV_MODE ? true : null,
      showEnsoDevtools: IS_DEV_MODE ? true : null,
      toggleEnsoDevtools: () => {
        set(({ showEnsoDevtools }) => ({ showEnsoDevtools: !(showEnsoDevtools ?? false) }))
      },
      setShowDevtools: (showDevtools) => {
        set({ showDevtools, showEnsoDevtools: showDevtools })
      },
      toggleDevtools: () => {
        set(({ showDevtools, showEnsoDevtools }) => {
          if (showEnsoDevtools === false) {
            return {
              showDevtools: true,
              showEnsoDevtools: true,
            }
          }

          return {
            showDevtools: !(showDevtools ?? false),
            showEnsoDevtools: !(showDevtools ?? false),
          }
        })
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
      animationsDisabled: localStorage.getItem('disableAnimations') === 'true',
      setAnimationsDisabled: (animationsDisabled) => {
        if (animationsDisabled) {
          localStorage.setItem('disableAnimations', 'true')
          MotionGlobalConfig.skipAnimations = true
          document.documentElement.classList.add('disable-animations')
        } else {
          localStorage.setItem('disableAnimations', 'false')
          MotionGlobalConfig.skipAnimations = false
          document.documentElement.classList.remove('disable-animations')
        }

        set({ animationsDisabled })
      },
    }),
    {
      name: 'ensoDevtools',
      partialize: (state) => ({
        showDevtools: state.showDevtools,
        showEnsoDevtools: state.showEnsoDevtools,
        animationsDisabled: state.animationsDisabled,
      }),
    },
  ),
)

// ===============================
// === useEnableVersionChecker ===
// ===============================

/** A function to set whether the version checker is forcibly shown/hidden. */
export function useEnableVersionChecker() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.showVersionChecker, {
    unsafeEnableTransition: true,
  })
}

// ==================================
// === useSetEnableVersionChecker ===
// ==================================

/** A function to set whether the version checker is forcibly shown/hidden. */
export function useSetEnableVersionChecker() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.setEnableVersionChecker, {
    unsafeEnableTransition: true,
  })
}

/** A function to get whether animations are disabled. */
export function useAnimationsDisabled() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.animationsDisabled, {
    unsafeEnableTransition: true,
  })
}

/** A function to set whether animations are disabled. */
export function useSetAnimationsDisabled() {
  return zustand.useStore(ensoDevtoolsStore, (state) => state.setAnimationsDisabled, {
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

if (typeof window !== 'undefined') {
  window.toggleDevtools = ensoDevtoolsStore.getState().toggleDevtools
}
