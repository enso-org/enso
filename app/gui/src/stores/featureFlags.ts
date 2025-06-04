import { type FeatureFlags, flagsStore } from '#/providers/FeatureFlagsProvider'
import { onScopeDispose, Ref, ref } from 'vue'

/** Composable for getting a specific feature flag. */
export function useFeatureFlag<Key extends keyof FeatureFlags>(key: Key): Ref<FeatureFlags[Key]> {
  const value: Ref<FeatureFlags[Key]> = ref(flagsStore.getState().featureFlags[key]) as Ref<
    FeatureFlags[Key]
  >

  const unsubscribe = flagsStore.subscribe((state) => {
    if (value.value !== state.featureFlags[key]) {
      value.value = state.featureFlags[key]
    }
  })
  onScopeDispose(unsubscribe)

  return value
}

export function setFeatureFlags(flags: Partial<FeatureFlags>) {
  return flagsStore.getState().setFeatureFlags(flags)
}

export function setFeatureFlag<Key extends keyof FeatureFlags>(key: Key, value: FeatureFlags[Key]) {
  return flagsStore.getState().setFeatureFlag(key, value)
}
