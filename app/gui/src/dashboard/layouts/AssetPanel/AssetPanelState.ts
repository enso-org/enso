/**
 * @file
 * The state of the asset panel. Can be used to control the asset panel's visibility,
 * selected tab, and other properties from outside the component.
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { AnyAsset, default as Backend } from '#/services/Backend'
import * as zustand from '#/utilities/zustand'
import { startTransition } from 'react'
import {
  storedAssetPanelTab,
  storedIsAssetPanelHidden,
  storedIsAssetPanelVisible,
} from './assetPanelLocalStorage'
import type { AssetPropertiesSpotlight } from './components/AssetProperties'
import type { AssetPanelTab } from './types'

/** The state of the asset panel. */
export interface AssetPanelState {
  readonly selectedTab: AssetPanelTab
  readonly setSelectedTab: (tab: AssetPanelTab) => void
  readonly isAssetPanelPermanentlyOpen: boolean
  readonly setIsAssetPanelOpen: (isAssetPanelOpen: boolean) => void
  readonly setIsAssetPanelPermanentlyOpen: (isAssetPanelPermanentlyOpen: boolean) => void
  readonly toggleIsAssetPanelPermanentlyOpen: () => void
  readonly isAssetPanelTemporarilyOpen: boolean
  readonly setIsAssetPanelTemporarilyOpen: (isAssetPanelTemporarilyOpen: boolean) => void
  readonly assetPanelProps: AssetPanelContextProps
  readonly setAssetPanelProps: (assetPanelProps: Partial<AssetPanelContextProps>) => void
  readonly isAssetPanelHidden: boolean
  readonly setIsAssetPanelHidden: (isAssetPanelHidden: boolean) => void
}

export const assetPanelStore = zustand.createStore<AssetPanelState>((set, get) => {
  return {
    selectedTab: storedAssetPanelTab.get() ?? 'settings',
    setSelectedTab: (tab) => {
      set({ selectedTab: tab })
      storedAssetPanelTab.set(tab)
    },
    isAssetPanelPermanentlyOpen: false,
    toggleIsAssetPanelPermanentlyOpen: () => {
      get().setIsAssetPanelPermanentlyOpen(!get().isAssetPanelPermanentlyOpen)
    },
    setIsAssetPanelPermanentlyOpen: (isAssetPanelPermanentlyOpen) => {
      if (get().isAssetPanelPermanentlyOpen !== isAssetPanelPermanentlyOpen) {
        set({ isAssetPanelPermanentlyOpen })
        storedIsAssetPanelVisible.set(isAssetPanelPermanentlyOpen)
      }
    },
    setIsAssetPanelOpen: (isAssetPanelExpanded) => {
      const state = get()

      if (state.isAssetPanelPermanentlyOpen !== isAssetPanelExpanded) {
        state.setIsAssetPanelPermanentlyOpen(isAssetPanelExpanded)
        state.setIsAssetPanelTemporarilyOpen(false)
      }

      if (state.isAssetPanelHidden && isAssetPanelExpanded) {
        state.setIsAssetPanelHidden(false)
      }
    },
    isAssetPanelTemporarilyOpen: false,
    setIsAssetPanelTemporarilyOpen: (isAssetPanelTemporarilyOpen) => {
      const state = get()

      if (state.isAssetPanelHidden && isAssetPanelTemporarilyOpen) {
        state.setIsAssetPanelHidden(false)
      }

      if (state.isAssetPanelTemporarilyOpen !== isAssetPanelTemporarilyOpen) {
        set({ isAssetPanelTemporarilyOpen })
      }
    },
    assetPanelProps: {
      selectedTab: storedAssetPanelTab.get() ?? 'settings',
      backend: null,
      item: null,
      spotlightOn: null,
      path: null,
    },
    setAssetPanelProps: (assetPanelProps) => {
      const current = get().assetPanelProps
      if (current !== assetPanelProps) {
        set({ assetPanelProps: { ...current, ...assetPanelProps } })
      }
    },
    isAssetPanelHidden: storedIsAssetPanelHidden.get() ?? false,
    setIsAssetPanelHidden: (isAssetPanelHidden) => {
      const state = get()

      if (state.isAssetPanelHidden !== isAssetPanelHidden) {
        set({ isAssetPanelHidden })
        storedIsAssetPanelHidden.set(isAssetPanelHidden)
      }
    },
  }
})

/** Props supplied by the row. */
export interface AssetPanelContextProps {
  readonly backend: Backend | null
  readonly selectedTab: AssetPanelTab
  readonly item: AnyAsset | null
  readonly path: string | null
  readonly spotlightOn: AssetPropertiesSpotlight | null
}

/** Whether the Asset Panel is toggled on. */
export function useIsAssetPanelPermanentlyOpen() {
  return zustand.useStore(assetPanelStore, (state) => state.isAssetPanelPermanentlyOpen, {
    unsafeEnableTransition: true,
  })
}

/** A function to set whether the Asset Panel is toggled on. */
export function useSetIsAssetPanelPermanentlyOpen() {
  return zustand.useStore(assetPanelStore, (state) => state.setIsAssetPanelPermanentlyOpen, {
    unsafeEnableTransition: true,
  })
}

/** Whether the Asset Panel is currently open (e.g. for editing a Datalink). */
export function useIsAssetPanelTemporarilyOpen() {
  return zustand.useStore(assetPanelStore, (state) => state.isAssetPanelTemporarilyOpen, {
    unsafeEnableTransition: true,
  })
}

/** A function to set whether the Asset Panel is currently open (e.g. for editing a Datalink). */
export function useSetIsAssetPanelTemporarilyOpen() {
  return zustand.useStore(assetPanelStore, (state) => state.setIsAssetPanelTemporarilyOpen, {
    unsafeEnableTransition: true,
  })
}

/** Whether the Asset Panel is open. */
export function useIsAssetPanelOpen() {
  return zustand.useStore(
    assetPanelStore,
    ({ isAssetPanelPermanentlyOpen, isAssetPanelTemporarilyOpen }) =>
      isAssetPanelPermanentlyOpen || isAssetPanelTemporarilyOpen,
    { unsafeEnableTransition: true },
  )
}

/** A function to set whether the Asset Panel is open. */
export function useSetIsAssetPanelOpen() {
  return zustand.useStore(assetPanelStore, (state) => state.setIsAssetPanelOpen, {
    unsafeEnableTransition: true,
  })
}

/** Props for the Asset Panel. */
export function useAssetPanelProps() {
  return zustand.useStore(assetPanelStore, (state) => state.assetPanelProps, {
    unsafeEnableTransition: true,
    areEqual: 'shallow',
  })
}

/** The selected tab of the Asset Panel. */
export function useAssetPanelSelectedTab() {
  return zustand.useStore(assetPanelStore, (state) => state.assetPanelProps.selectedTab, {
    unsafeEnableTransition: true,
  })
}

/** A function to set props for the Asset Panel. */
export function useSetAssetPanelProps() {
  return zustand.useStore(assetPanelStore, (state) => state.setAssetPanelProps, {
    unsafeEnableTransition: true,
  })
}

/** A function to reset the Asset Panel props to their default values. */
export function useResetAssetPanelProps() {
  return useEventCallback(() => {
    const current = assetPanelStore.getState().assetPanelProps
    if (current.item != null) {
      assetPanelStore.setState({
        assetPanelProps: {
          selectedTab: current.selectedTab,
          backend: null,
          item: null,
          spotlightOn: null,
          path: null,
        },
      })
    }
  })
}

/** A function to set the selected tab of the Asset Panel. */
export function useSetAssetPanelSelectedTab() {
  return useEventCallback((selectedTab: AssetPanelContextProps['selectedTab']) => {
    startTransition(() => {
      const current = assetPanelStore.getState().assetPanelProps
      if (current.selectedTab !== selectedTab) {
        assetPanelStore.setState({
          assetPanelProps: { ...current, selectedTab },
        })
      }
    })
  })
}

/** Whether the Asset Panel is hidden. */
export function useIsAssetPanelHidden() {
  return zustand.useStore(assetPanelStore, (state) => state.isAssetPanelHidden, {
    unsafeEnableTransition: true,
  })
}

/** A function to set whether the Asset Panel is hidden. */
export function useSetIsAssetPanelHidden() {
  return zustand.useStore(assetPanelStore, (state) => state.setIsAssetPanelHidden, {
    unsafeEnableTransition: true,
  })
}
