import { useAuth } from '$/providers/auth'
import { useDevtoolsStore } from '$/providers/devTools'
import { toRef } from 'vue'
import {
  getFeatureConfiguration,
  mapPlanOnPaywall,
  type PaywallFeatureName,
} from './FeaturesConfiguration'

/**
 * A composable returning function checking if the given feature is under current user's paywall.
 */
export function useIsFeatureUnderPaywall() {
  const auth = useAuth()
  const features = toRef(useDevtoolsStore(), 'paywallFeatures')

  return (feature: PaywallFeatureName, ignoreForceEnabled = false) => {
    const plan = auth.session?.user.plan
    const paywallLevel = plan ? mapPlanOnPaywall(plan) : undefined
    const featureConfig = getFeatureConfiguration(feature)
    const isForceEnabled = ignoreForceEnabled ? null : features.value[feature].isForceEnabled
    const { level } = featureConfig

    if (isForceEnabled == null) {
      return paywallLevel != null ? level > paywallLevel : true
    } else {
      return !isForceEnabled
    }
  }
}
