/**
 * @file
 * The state of the asset panel. Can be used to control the asset panel's visibility,
 * selected tab, and other properties from outside the component.
 */
import { startTransition } from 'react'

import { z } from 'zod'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type Backend from '#/services/Backend'
import type { AnyAsset } from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import * as zustand from '#/utilities/zustand'
import type { AssetPropertiesSpotlight } from './components/AssetProperties'
import { ASSET_PANEL_TABS, type AssetPanelTab } from './types'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
    readonly isAssetPanelHidden: boolean
    readonly assetPanelTab: AssetPanelTab
    readonly assetPanelWidth: number
  }
}

const ASSET_PANEL_TAB_SCHEMA = z.enum(ASSET_PANEL_TABS)

LocalStorage.register({
  assetPanelTab: { schema: ASSET_PANEL_TAB_SCHEMA },
  assetPanelWidth: { schema: z.number().int() },
  isAssetPanelHidden: { schema: z.boolean() },
  isAssetPanelVisible: { schema: z.boolean() },
})

/** The state of the asset panel. */
export interface AssetPanelState {
  readonly selectedTab: AssetPanelTab
  readonly setSelectedTab: (tab: AssetPanelTab) => void
  readonly isAssetPanelPermanentlyVisible: boolean
  readonly isAssetPanelExpanded: boolean
  readonly setIsAssetPanelExpanded: (isAssetPanelExpanded: boolean) => void
  readonly setIsAssetPanelPermanentlyVisible: (isAssetPanelTemporarilyVisible: boolean) => void
  readonly toggleIsAssetPanelPermanentlyVisible: () => void
  readonly isAssetPanelTemporarilyVisible: boolean
  readonly setIsAssetPanelTemporarilyVisible: (isAssetPanelTemporarilyVisible: boolean) => void
  readonly assetPanelProps: AssetPanelContextProps
  readonly setAssetPanelProps: (assetPanelProps: Partial<AssetPanelContextProps>) => void
  readonly isAssetPanelHidden: boolean
  readonly setIsAssetPanelHidden: (isAssetPanelHidden: boolean) => void
}

export const assetPanelStore = zustand.createStore<AssetPanelState>((set, get) => {
  const localStorage = LocalStorage.getInstance()
  return {
    selectedTab: localStorage.get('assetPanelTab') ?? 'settings',
    setSelectedTab: (tab) => {
      set({ selectedTab: tab })
      localStorage.set('assetPanelTab', tab)
    },
    isAssetPanelPermanentlyVisible: false,
    toggleIsAssetPanelPermanentlyVisible: () => {
      const state = get()
      const next = !state.isAssetPanelPermanentlyVisible

      state.setIsAssetPanelPermanentlyVisible(next)
    },
    setIsAssetPanelPermanentlyVisible: (isAssetPanelPermanentlyVisible) => {
      if (get().isAssetPanelPermanentlyVisible !== isAssetPanelPermanentlyVisible) {
        set({ isAssetPanelPermanentlyVisible })
        localStorage.set('isAssetPanelVisible', isAssetPanelPermanentlyVisible)
      }
    },
    isAssetPanelExpanded: false,
    setIsAssetPanelExpanded: (isAssetPanelExpanded) => {
      const state = get()

      if (state.isAssetPanelPermanentlyVisible !== isAssetPanelExpanded) {
        state.setIsAssetPanelPermanentlyVisible(isAssetPanelExpanded)
        state.setIsAssetPanelTemporarilyVisible(false)
      }

      if (state.isAssetPanelHidden && isAssetPanelExpanded) {
        state.setIsAssetPanelHidden(false)
      }
    },
    isAssetPanelTemporarilyVisible: false,
    setIsAssetPanelTemporarilyVisible: (isAssetPanelTemporarilyVisible) => {
      const state = get()

      if (state.isAssetPanelHidden && isAssetPanelTemporarilyVisible) {
        state.setIsAssetPanelHidden(false)
      }

      if (state.isAssetPanelTemporarilyVisible !== isAssetPanelTemporarilyVisible) {
        set({ isAssetPanelTemporarilyVisible })
      }
    },
    assetPanelProps: {
      selectedTab: localStorage.get('assetPanelTab') ?? 'settings',
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
    isAssetPanelHidden: localStorage.get('isAssetPanelHidden') ?? false,
    setIsAssetPanelHidden: (isAssetPanelHidden) => {
      const state = get()

      if (state.isAssetPanelHidden !== isAssetPanelHidden) {
        set({ isAssetPanelHidden })
        localStorage.set('isAssetPanelHidden', isAssetPanelHidden)
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
export function useIsAssetPanelPermanentlyVisible() {
  return zustand.useStore(assetPanelStore, (state) => state.isAssetPanelPermanentlyVisible, {
    unsafeEnableTransition: true,
  })
}

/** A function to set whether the Asset Panel is toggled on. */
export function useSetIsAssetPanelPermanentlyVisible() {
  return zustand.useStore(assetPanelStore, (state) => state.setIsAssetPanelPermanentlyVisible, {
    unsafeEnableTransition: true,
  })
}

/** Whether the Asset Panel is currently visible (e.g. for editing a Datalink). */
export function useIsAssetPanelTemporarilyVisible() {
  return zustand.useStore(assetPanelStore, (state) => state.isAssetPanelTemporarilyVisible, {
    unsafeEnableTransition: true,
  })
}

/** A function to set whether the Asset Panel is currently visible (e.g. for editing a Datalink). */
export function useSetIsAssetPanelTemporarilyVisible() {
  return zustand.useStore(assetPanelStore, (state) => state.setIsAssetPanelTemporarilyVisible, {
    unsafeEnableTransition: true,
  })
}

/** Whether the Asset Panel is currently visible, either temporarily or permanently. */
export function useIsAssetPanelVisible() {
  const isAssetPanelPermanentlyVisible = useIsAssetPanelPermanentlyVisible()
  const isAssetPanelTemporarilyVisible = useIsAssetPanelTemporarilyVisible()
  return isAssetPanelPermanentlyVisible || isAssetPanelTemporarilyVisible
}

/** Whether the Asset Panel is expanded. */
export function useIsAssetPanelExpanded() {
  return zustand.useStore(
    assetPanelStore,
    ({ isAssetPanelPermanentlyVisible, isAssetPanelTemporarilyVisible }) =>
      isAssetPanelPermanentlyVisible || isAssetPanelTemporarilyVisible,
    { unsafeEnableTransition: true },
  )
}

/** A function to set whether the Asset Panel is expanded. */
export function useSetIsAssetPanelExpanded() {
  return zustand.useStore(assetPanelStore, (state) => state.setIsAssetPanelExpanded, {
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
