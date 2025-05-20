/** @file Global store for auth state. */
import { useStore } from '#/hooks/storeHooks'
import type { Plan } from '#/services/Backend'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

/** State for {@link authOverridesStore}. */
interface AuthOverridesStoreState {
  readonly planOverride: Plan | undefined
  readonly setPlanOverride: (planOverride: Plan | undefined) => void
}

const authOverridesStore = createStore<AuthOverridesStoreState>()(
  persist(
    (set): AuthOverridesStoreState => ({
      planOverride: undefined,
      setPlanOverride: (planOverride) => {
        set({ planOverride })
      },
    }),
    { name: 'enso-auth-overrides', version: 1 },
  ),
)

/** The current overridden plan. */
export function usePlanOverride() {
  return useStore(authOverridesStore, ({ planOverride }) => planOverride)
}

/** A function to set (or unset) the current overridden plan. */
export function useSetPlanOverride() {
  return useStore(authOverridesStore, ({ setPlanOverride }) => setPlanOverride)
}
