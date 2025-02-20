/**
 * @file
 *
 * Feature flags provider.
 * Feature flags are used to enable or disable certain features in the application.
 */
import { unsafeWriteValue } from '#/utilities/write'
import { createStore, useStore } from '#/utilities/zustand'
import { IS_DEV_MODE, isOnElectron } from 'enso-common/src/detect'
import { z } from 'zod'
import { persist } from 'zustand/middleware'

const MIN_ASSETS_TABLE_REFRESH_INTERVAL_MS = 100
const DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS = 3_000

/**
 * Feature flags for internal testing.
 */
export function featureFlagsForInternalTesting() {
  return {
    enableCloudExecution: true,
    enableAsyncExecution: true,
    enableAdvancedProjectExecutionOptions: true,
  }
}

export const FEATURE_FLAGS_SCHEMA = z.object({
  enableMultitabs: z.boolean(),
  enableAssetsTableBackgroundRefresh: z.boolean(),
  assetsTableBackgroundRefreshInterval: z.number().min(MIN_ASSETS_TABLE_REFRESH_INTERVAL_MS),
  enableCloudExecution: z.boolean(),
  enableAsyncExecution: z.boolean(),
  enableAdvancedProjectExecutionOptions: z.boolean(),
})

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

const flagsStore = createStore<FeatureFlagsStore>()(
  persist(
    (set) => ({
      featureFlags: {
        enableMultitabs: false,
        enableAssetsTableBackgroundRefresh: true,
        assetsTableBackgroundRefreshInterval: DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS,
        enableCloudExecution: IS_DEV_MODE || isOnElectron(),
        enableAsyncExecution: false,
        enableAdvancedProjectExecutionOptions: true,
      },
      setFeatureFlag: (key, value) => {
        set(({ featureFlags }) => ({ featureFlags: { ...featureFlags, [key]: value } }))
      },
      setFeatureFlags: (flags) => {
        set(({ featureFlags }) => ({ featureFlags: { ...featureFlags, ...flags } }))
      },
    }),
    {
      name: 'featureFlags',
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

        const parsedPersistedState = FEATURE_FLAGS_SCHEMA.safeParse(persistedState)

        if (parsedPersistedState.success) {
          unsafeMutateFeatureFlags(parsedPersistedState.data)
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
