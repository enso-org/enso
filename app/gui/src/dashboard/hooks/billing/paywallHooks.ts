/**
 * @file
 *
 * Hooks for paywall-related functionality.
 */
import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as devtools from '#/components/Devtools'

import type * as backend from 'enso-common/src/services/Backend'

import * as React from 'react'
import * as paywallConfiguration from './FeaturesConfiguration'
import * as paywallFeatures from './paywallFeaturesHooks'

/** Props for the {@link usePaywall} hook. */
export interface UsePaywallProps {
  readonly plan: backend.Plan
}

/** A hook that provides paywall-related functionality. */
export function usePaywall(props: UsePaywallProps) {
  const { plan } = props

  const { getFeature } = paywallFeatures.usePaywallFeatures()
  const { features } = devtools.usePaywallDevtools()
  const paywallLevel = paywallConfiguration.mapPlanOnPaywall(plan)

  const getPaywallLevel = eventCallbackHooks.useEventCallback((specifiedPlan: backend.Plan) =>
    paywallConfiguration.mapPlanOnPaywall(specifiedPlan),
  )

  const isFeatureUnderPaywall = React.useCallback(
    (feature: paywallConfiguration.PaywallFeatureName, ignoreForceEnabled = false) => {
      const featureConfig = getFeature(feature)
      const isForceEnabled = ignoreForceEnabled ? null : features[feature].isForceEnabled
      const { level } = featureConfig

      if (isForceEnabled == null) {
        return level > paywallLevel
      } else {
        return !isForceEnabled
      }
    },
    [paywallLevel, getFeature, features],
  )

  return {
    paywallLevel,
    isFeatureUnderPaywall,
    getPaywallLevel,
    getFeature,
  } as const
}
