/**
 * @file
 *
 * Feature flags provider.
 * Feature flags are used to enable or disable certain features in the application.
 */
import { unsafeWriteValue } from '#/utilities/write'
import { useZustantStoreRef } from '$/utils/zustand'
import { IS_DEV_MODE, isOnElectron } from 'enso-common/src/detect'
import { z } from 'zod'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

const MIN_ASSETS_TABLE_REFRESH_INTERVAL_MS = 100
export const DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS = 3_000

/** Feature flags for internal testing. */
export function featureFlagsForInternalTesting() {
  return {
    enableCloudExecution: true,
    enableScheduledExecution: true,
    enableAdvancedProjectExecutionOptions: false,
    enableHybridExecution: true,
  }
}

export const FEATURE_FLAGS_SCHEMA = z.object({
  enableMultitabs: z.boolean(),
  enableAssetsTableBackgroundRefresh: z.boolean(),
  assetsTableBackgroundRefreshInterval: z.number().min(MIN_ASSETS_TABLE_REFRESH_INTERVAL_MS),
  enableCloudExecution: z.boolean(),
  enableScheduledExecution: z.boolean(),
  enableAdvancedProjectExecutionOptions: z.boolean(),
  enableHybridExecution: z.boolean(),
  showDeveloperIds: z.boolean(),
  overrideProfilePicture: z.boolean(),
  multiplyUserList: z.boolean(),
})

const FEATURE_FLAGS_STATE_SCHEMA = z.object({ featureFlags: FEATURE_FLAGS_SCHEMA })

/** Feature flags. */
export type FeatureFlags = z.infer<typeof FEATURE_FLAGS_SCHEMA>

/** Feature flags store. */
export interface FeatureFlagsStore {
  readonly featureFlags: FeatureFlags
  readonly setFeatureFlag: <Key extends keyof FeatureFlags>(
    key: Key,
    value: FeatureFlags[Key],
  ) => void
  readonly setFeatureFlags: (flags: Partial<FeatureFlags>) => void
}

export const flagsStore = createStore<FeatureFlagsStore>()(
  persist(
    (set) => ({
      featureFlags: {
        enableMultitabs: false,
        enableAssetsTableBackgroundRefresh: true,
        assetsTableBackgroundRefreshInterval: DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS,
        enableCloudExecution: IS_DEV_MODE || isOnElectron(),
        enableScheduledExecution: true,
        enableAdvancedProjectExecutionOptions: false,
        enableHybridExecution: IS_DEV_MODE,
        showDeveloperIds: false,
        overrideProfilePicture: false,
        multiplyUserList: false,
      },
      setFeatureFlag: (key, value) => {
        set(({ featureFlags }) => ({ featureFlags: { ...featureFlags, [key]: value } }))
      },
      setFeatureFlags: (flags) => {
        set(({ featureFlags }) => ({ featureFlags: { ...featureFlags, ...flags } }))
      },
    }),
    {
      name: 'enso-feature-flags',
      version: 1,
      merge: (persistedState, newState) => {
        /**
         * Mutates the state with provided feature flags
         */
        function unsafeMutateFeatureFlags(flags: Partial<FeatureFlags>) {
          unsafeWriteValue(newState, 'featureFlags', {
            ...newState.featureFlags,
            ...flags,
          })
        }

        const parsedPersistedState = FEATURE_FLAGS_STATE_SCHEMA.safeParse(persistedState)

        if (parsedPersistedState.success === true) {
          unsafeMutateFeatureFlags(parsedPersistedState.data.featureFlags)
        }

        if (typeof window !== 'undefined') {
          const predefinedFeatureFlags = FEATURE_FLAGS_SCHEMA.partial().safeParse(
            window.overrideFeatureFlags,
          )

          if (predefinedFeatureFlags.success) {
            const withOmittedUndefined = Object.fromEntries(
              Object.entries(predefinedFeatureFlags.data).filter(([, value]) => value != null),
            )
            // This is safe, because zod omits unset values.
            unsafeMutateFeatureFlags(withOmittedUndefined)
          }
        }

        return newState
      },
    },
  ),
)

/** Composable for getting a specific feature flag. */
export function useFeatureFlag<Key extends keyof FeatureFlags>(key: Key) {
  return useZustantStoreRef(flagsStore, (store) => store.featureFlags[key])
}

/** Set a subset of feature flags. */
export function setFeatureFlags(flags: Partial<FeatureFlags>) {
  return flagsStore.getState().setFeatureFlags(flags)
}

/** Set a single feature flag. */
export function setFeatureFlag<Key extends keyof FeatureFlags>(key: Key, value: FeatureFlags[Key]) {
  return flagsStore.getState().setFeatureFlag(key, value)
}

// Define global API for managing feature flags
if (typeof window !== 'undefined') {
  Object.defineProperty(window, 'featureFlags', {
    value: flagsStore.getState().featureFlags,
    configurable: false,
    writable: false,
  })

  Object.defineProperty(window, 'setFeatureFlags', {
    value: flagsStore.getState().setFeatureFlags,
    configurable: false,
    writable: false,
  })
}
