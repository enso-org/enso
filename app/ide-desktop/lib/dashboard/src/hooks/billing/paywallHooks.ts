/**
 * @file
 *
 * Hooks for paywall-related functionality.
 */
import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as paywall from '#/components/Paywall'

import type * as backend from '#/services/Backend'

import * as paywallConfiguration from './FeaturesConfiguration'
import * as paywallFeatures from './paywallFeaturesHooks'

/**
 * Props for the {@link usePaywall} hook.
 */
export interface UsePaywallProps {
  readonly plan?: backend.Plan | undefined
}

/**
 * A hook that provides paywall-related functionality.
 */
export function usePaywall(props: UsePaywallProps) {
  const { plan } = props

  const { getFeature } = paywallFeatures.usePaywallFeatures()
  const { features } = paywall.usePaywallDevtools()

  const getPaywallLevel = eventCallbackHooks.useEventCallback(() =>
    paywallConfiguration.mapPlanOnPaywall(plan)
  )

  const isFeatureUnderPaywall = eventCallbackHooks.useEventCallback(
    (feature: paywallConfiguration.PaywallFeatureName) => {
      const featureConfig = getFeature(feature)
      const { isForceEnabled } = features[feature]
      const { level } = featureConfig
      const paywallLevel = getPaywallLevel()

      if (isForceEnabled == null) {
        return paywallLevel >= level
      } else {
        return !isForceEnabled
      }
    }
  )

  return { isFeatureUnderPaywall, getPaywallLevel, getFeature } as const
}
