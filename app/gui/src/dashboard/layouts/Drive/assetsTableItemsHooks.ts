/** @file A hook to return the items in the assets table. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { createStore, useStore } from '#/utilities/zustand.ts'
import type { AnyAsset, AssetId } from 'enso-common/src/services/Backend'
import type { DirectoryId } from 'enso-common/src/services/ProjectManager/types'
import { startTransition, useEffect } from 'react'

/** Options for {@link useAssetsTableItems}. */
export interface UseAssetsTableOptions {
  readonly parentId: DirectoryId
  readonly assets: readonly AnyAsset[]
}

export const ASSET_ITEMS_STORE = createStore<{
  readonly items: ReadonlyMap<AssetId, AnyAsset>
  readonly setItems: (parentId: DirectoryId, items: readonly AnyAsset[]) => void
}>((set) => ({
  items: new Map(),
  setItems: (parentId, items) => {
    set(({ items: oldItems }) => ({
      items: new Map([
        ...[...oldItems.entries()].filter(([, item]) => item.parentId !== parentId),
        ...items.map((item) => [item.id, item] as const),
      ]),
    }))
  },
}))

/** Return the asset with the given id. */
export function useAsset(id: AssetId) {
  return useStore(ASSET_ITEMS_STORE, (store) => store.items.get(id) ?? null, {
    unsafeEnableTransition: true,
  })
}

/** Return a function to retrieve an arbitrary asset. */
export function useGetAsset() {
  return useEventCallback((id: AssetId) => ASSET_ITEMS_STORE.getState().items.get(id))
}

/** Return a function to retrieve an arbitrary asset. */
export function useGetAssetChildren() {
  return useEventCallback((parentId: DirectoryId) =>
    [...ASSET_ITEMS_STORE.getState().items.values()].filter((asset) => asset.parentId === parentId),
  )
}

/** A hook to return the items in the assets table. */
export function useAssetsTableItems(options: UseAssetsTableOptions) {
  const { parentId, assets } = options

  useEffect(() => {
    startTransition(() => {
      ASSET_ITEMS_STORE.getState().setItems(parentId, assets)
    })
  }, [assets, parentId])
}
