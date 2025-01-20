/**
 * @file
 *
 * Hooks for paywall features.
 */

import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as paywallFeaturesConfiguration from './FeaturesConfiguration'

/** A hook that provides access to the paywall features configuration. */
export function usePaywallFeatures() {
  const getFeature = eventCallbackHooks.useEventCallback(
    <Key extends paywallFeaturesConfiguration.PaywallFeatureName>(feature: Key) => {
      return paywallFeaturesConfiguration.getFeatureConfiguration<Key>(feature)
    },
  )

  const valueIsFeature = eventCallbackHooks.useEventCallback(
    (value: string): value is paywallFeaturesConfiguration.PaywallFeatureName =>
      value in paywallFeaturesConfiguration.PAYWALL_FEATURES,
  )

  const getMaybeFeature = eventCallbackHooks.useEventCallback((feature: string) =>
    valueIsFeature(feature) ? getFeature(feature) : null,
  )

  return {
    getFeature,
    valueIsFeature,
    getMaybeFeature,
  } as const
}
