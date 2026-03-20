/** @file A zustand store that contains the state of the Enso devtools. */
import {
  PAYWALL_FEATURES,
  type PaywallFeatureName,
} from '$/composables/paywall/FeaturesConfiguration'
import { createGlobalState } from '@vueuse/core'
import { unsafeEntries, unsafeFromEntries } from 'enso-common/src/utilities/data/object'
import { IS_DEV_MODE } from 'enso-common/src/utilities/detect'
import { reactive } from 'vue'

/** Configuration for a paywall feature. */
export interface PaywallDevtoolsFeatureConfiguration {
  readonly isForceEnabled: boolean | null
}

/** A store keeping state of Enso and React devtools. */
export type EnsoDevtoolsStore = ReturnType<typeof createDevtoolsStore>

/** create {@link EnsoDevtoolsStore} */
export function createDevtoolsStore() {
  const paywallFeatures: Record<PaywallFeatureName, PaywallDevtoolsFeatureConfiguration> =
    unsafeFromEntries(
      unsafeEntries(PAYWALL_FEATURES).map(([feature]) => [feature, { isForceEnabled: null }]),
    )

  function setPaywallFeature(feature: PaywallFeatureName, isForceEnabled: boolean | null) {
    paywallFeatures[feature] = { isForceEnabled }
  }

  return reactive({
    showDevtools: IS_DEV_MODE,
    showEnsoDevtools: IS_DEV_MODE,
    showVersionChecker: false as boolean | null,
    paywallFeatures,
    setPaywallFeature,
  })
}

export const useDevtoolsStore = createGlobalState(createDevtoolsStore)
