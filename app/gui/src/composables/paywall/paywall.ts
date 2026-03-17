import { useAuth } from '$/providers/auth'
import { useDevtoolsStore } from '$/providers/devTools'
import { toRef } from 'vue'
import {
  getFeatureConfiguration,
  mapPlanOnPaywall,
  type PaywallFeatureName,
} from './FeaturesConfiguration'

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
