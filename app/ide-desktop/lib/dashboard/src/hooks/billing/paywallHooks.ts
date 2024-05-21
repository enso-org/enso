/**
 * @file
 *
 * Hooks for paywall-related functionality.
 */
import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

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

  const getPaywallLevel = eventCallbackHooks.useEventCallback(() =>
    paywallConfiguration.mapPlanOnPaywall(plan)
  )

  const isFeatureUnderPaywall = eventCallbackHooks.useEventCallback(
    (feature: paywallConfiguration.PaywallFeatureName) => {
      const featureConfig = getFeature(feature)
      const { level } = featureConfig
      const paywallLevel = getPaywallLevel()

      return plan !== undefined && paywallLevel >= level
    }
  )

  return { isFeatureUnderPaywall, getPaywallLevel } as const
}
