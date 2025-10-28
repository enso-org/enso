import { useStore } from '#/utilities/zustand'
import { flagsStore, type FeatureFlagsStore } from '$/providers/featureFlags'

/** Hook to get all feature flags. */
export function useFeatureFlags() {
  return useStore(flagsStore, (state) => state.featureFlags)
}

/** Hook to get a specific feature flag. */
export function useFeatureFlag<Key extends keyof FeatureFlagsStore['featureFlags']>(
  key: Key,
): FeatureFlagsStore['featureFlags'][Key] {
  return useStore(flagsStore, ({ featureFlags }) => featureFlags[key])
}

/** Hook to set feature flags. */
export function useSetFeatureFlags() {
  return useStore(flagsStore, ({ setFeatureFlags }) => setFeatureFlags)
}

/** Hook to set a specific feature flag. */
export function useSetFeatureFlag() {
  return useStore(flagsStore, ({ setFeatureFlag }) => setFeatureFlag)
}
