/**
 * @file
 *
 * Feature flags provider.
 * Feature flags are used to enable or disable certain features in the application.
 */
import { useMount } from '#/hooks/mountHooks'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import LocalStorage from '#/utilities/LocalStorage'
import { unsafeEntries } from '#/utilities/object'
import type { ReactNode } from 'react'
import { useEffect } from 'react'
import { z } from 'zod'
import { createStore, useStore } from 'zustand'

declare module '#/utilities/LocalStorage' {
  /** Local storage data structure. */
  interface LocalStorageData {
    readonly featureFlags: z.infer<typeof FEATURE_FLAGS_SCHEMA>
  }
}

export const FEATURE_FLAGS_SCHEMA = z.object({
  enableMultitabs: z.boolean(),
  enableAssetsTableBackgroundRefresh: z.boolean(),
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  assetsTableBackgroundRefreshInterval: z.number().min(100),
})

LocalStorage.registerKey('featureFlags', { schema: FEATURE_FLAGS_SCHEMA })

/** Feature flags store. */
export interface FeatureFlags {
  readonly featureFlags: {
    readonly enableMultitabs: boolean
    readonly enableAssetsTableBackgroundRefresh: boolean
    readonly assetsTableBackgroundRefreshInterval: number
  }
  readonly setFeatureFlags: <Key extends keyof FeatureFlags['featureFlags']>(
    key: Key,
    value: FeatureFlags['featureFlags'][Key],
  ) => void
}

const flagsStore = createStore<FeatureFlags>((set) => ({
  featureFlags: {
    enableMultitabs: false,
    enableAssetsTableBackgroundRefresh: true,
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    assetsTableBackgroundRefreshInterval: 3_000,
  },
  setFeatureFlags: (key, value) => {
    set(({ featureFlags }) => ({ featureFlags: { ...featureFlags, [key]: value } }))
  },
}))

/** Hook to get all feature flags. */
export function useFeatureFlags() {
  return useStore(flagsStore, (state) => state.featureFlags)
}

/** Hook to get a specific feature flag. */
export function useFeatureFlag<Key extends keyof FeatureFlags['featureFlags']>(
  key: Key,
): FeatureFlags['featureFlags'][Key] {
  return useStore(flagsStore, ({ featureFlags }) => featureFlags[key])
}

/** Hook to set feature flags. */
export function useSetFeatureFlags() {
  return useStore(flagsStore, ({ setFeatureFlags }) => setFeatureFlags)
}

/**
 * Feature flags provider.
 * Gets feature flags from local storage and sets them in the store.
 * Also saves feature flags to local storage when they change.
 */
export function FeatureFlagsProvider({ children }: { children: ReactNode }) {
  const { localStorage } = useLocalStorage()
  const setFeatureFlags = useSetFeatureFlags()

  useMount(() => {
    const storedFeatureFlags = localStorage.get('featureFlags')

    if (storedFeatureFlags) {
      for (const [key, value] of unsafeEntries(storedFeatureFlags)) {
        setFeatureFlags(key, value)
      }
    }
  })

  useEffect(
    () =>
      flagsStore.subscribe((state, prevState) => {
        if (state.featureFlags !== prevState.featureFlags) {
          localStorage.set('featureFlags', state.featureFlags)
        }
      }),
    [localStorage],
  )

  return <>{children}</>
}
