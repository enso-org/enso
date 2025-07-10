/** @file A hook to return the items in the assets table. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { assetCompareFunction } from '#/layouts/Drive/compareAssets'
import type { DirectoryId } from '#/services/ProjectManager'
import type { SortInfo } from '#/utilities/sorting'
import { createStore, useStore } from '#/utilities/zustand.ts'
import { useText } from '$/providers/react'
import type { AnyAsset, AssetId, AssetSortExpression } from 'enso-common/src/services/Backend'
import { startTransition, useEffect } from 'react'

/** Options for {@link useAssetsTableItems}. */
export interface UseAssetsTableOptions {
  readonly parentId: DirectoryId
  readonly assets: readonly AnyAsset[]
  readonly sortInfo: SortInfo<AssetSortExpression> | null
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
  const { parentId, assets: items, sortInfo } = options
  const { locale } = useText()
  const setAssetItems = useStore(ASSET_ITEMS_STORE, (store) => store.setItems, {
    unsafeEnableTransition: true,
  })
  useEffect(() => {
    startTransition(() => {
      setAssetItems(parentId, items)
    })
  }, [items, parentId, setAssetItems])
  const compare = sortInfo ? assetCompareFunction(sortInfo, locale) : null
  const visibleItems = compare ? [...items].sort(compare) : items
  return { visibleItems } as const
}
