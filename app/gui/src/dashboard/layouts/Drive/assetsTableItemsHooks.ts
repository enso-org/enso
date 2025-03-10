/** @file A hook to return the items in the assets table. */
import type { AnyAsset, AssetId } from 'enso-common/src/services/Backend'
import { AssetType, getAssetPermissionName } from 'enso-common/src/services/Backend'
import { PermissionAction } from 'enso-common/src/utilities/permissions'

import type { SortableColumn } from '#/components/dashboard/column/columnUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { assetCompareFunction } from '#/layouts/Drive/compareAssets'
import { useText } from '#/providers/TextProvider'
import type { DirectoryId } from '#/services/ProjectManager'
import type AssetQuery from '#/utilities/AssetQuery'
import { fileExtension } from '#/utilities/fileInfo'
import type { SortInfo } from '#/utilities/sorting'
import { regexEscape } from '#/utilities/string'
import { createStore, useStore } from '#/utilities/zustand.ts'
import { useEffect } from 'react'
import invariant from 'tiny-invariant'

/** Options for {@link useAssetsTableItems}. */
export interface UseAssetsTableOptions {
  readonly parentId: DirectoryId
  readonly assets: readonly AnyAsset[]
  readonly sortInfo: SortInfo<SortableColumn> | null
  readonly query: AssetQuery
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

/** Return the asset with the given id, or throw an error if it is `undefined`. */
export function useAssetStrict(id: AssetId) {
  const asset = useAsset(id)
  invariant(
    asset,
    `Expected asset to be defined, but got undefined, Asset ID: ${JSON.stringify(id)}`,
  )
  return asset
}

/** A hook to return the items in the assets table. */
export function useAssetsTableItems(options: UseAssetsTableOptions) {
  const { parentId, assets: items, sortInfo, query } = options

  const { locale } = useText()
  const setAssetItems = useStore(ASSET_ITEMS_STORE, (store) => store.setItems)

  const filter = (() => {
    const globCache: Record<string, RegExp> = {}
    if (/^\s*$/.test(query.query)) {
      return null
    } else {
      return (asset: AnyAsset) => {
        if (asset.type === AssetType.specialEmpty || asset.type === AssetType.specialLoading) {
          return false
        }
        const assetType =
          asset.type === AssetType.directory ? 'folder'
          : asset.type === AssetType.datalink ? 'datalink'
          : String(asset.type)
        const assetExtension =
          asset.type !== AssetType.file ? null : fileExtension(asset.title).toLowerCase()
        const assetModifiedAt = new Date(asset.modifiedAt)
        const nodeLabels: readonly string[] = asset.labels ?? []
        const lowercaseName = asset.title.toLowerCase()
        const lowercaseDescription = asset.description?.toLowerCase() ?? ''
        const owners =
          asset.permissions
            ?.filter((permission) => permission.permission === PermissionAction.own)
            .map(getAssetPermissionName) ?? []
        const globMatch = (glob: string, match: string) => {
          const regex = (globCache[glob] =
            globCache[glob] ??
            new RegExp('^' + regexEscape(glob).replace(/(?:\\\*)+/g, '.*') + '$', 'i'))
          return regex.test(match)
        }
        const isAbsent = (type: string) => {
          switch (type) {
            case 'label':
            case 'labels': {
              return nodeLabels.length === 0
            }
            case 'name': {
              // Should never be true, but handle it just in case.
              return lowercaseName === ''
            }
            case 'description': {
              return lowercaseDescription === ''
            }
            case 'extension': {
              // Should never be true, but handle it just in case.
              return assetExtension === ''
            }
          }
          // Things like `no:name` and `no:owner` are never true.
          return false
        }
        const parseDate = (date: string) => {
          const lowercase = date.toLowerCase()
          switch (lowercase) {
            case 'today': {
              return new Date()
            }
          }
          return new Date(date)
        }
        const matchesDate = (date: string) => {
          const parsed = parseDate(date)
          return (
            parsed.getFullYear() === assetModifiedAt.getFullYear() &&
            parsed.getMonth() === assetModifiedAt.getMonth() &&
            parsed.getDate() === assetModifiedAt.getDate()
          )
        }
        const isEmpty = (values: string[]) =>
          values.length === 0 || (values.length === 1 && values[0] === '')

        const filterTag = (
          positive: string[][],
          negative: string[][],
          predicate: (value: string) => boolean,
        ) =>
          positive.every((values) => isEmpty(values) || values.some(predicate)) &&
          negative.every((values) => !values.some(predicate))

        return (
          filterTag(query.nos, query.negativeNos, (no) => isAbsent(no.toLowerCase())) &&
          filterTag(query.keywords, query.negativeKeywords, (keyword) =>
            lowercaseName.includes(keyword.toLowerCase()),
          ) &&
          filterTag(query.names, query.negativeNames, (name) => globMatch(name, lowercaseName)) &&
          filterTag(query.labels, query.negativeLabels, (label) =>
            nodeLabels.some((assetLabel) => globMatch(label, assetLabel)),
          ) &&
          filterTag(query.types, query.negativeTypes, (type) => type === assetType) &&
          filterTag(
            query.extensions,
            query.negativeExtensions,
            (extension) => extension.toLowerCase() === assetExtension,
          ) &&
          filterTag(query.descriptions, query.negativeDescriptions, (description) =>
            lowercaseDescription.includes(description.toLowerCase()),
          ) &&
          filterTag(query.modifieds, query.negativeModifieds, matchesDate) &&
          filterTag(query.owners, query.negativeOwners, (owner) =>
            owners.some((assetOwner) => globMatch(owner, assetOwner)),
          )
        )
      }
    }
  })()

  useEffect(() => {
    setAssetItems(parentId, items)
  }, [items, parentId, setAssetItems])

  const compare = sortInfo ? assetCompareFunction(sortInfo, locale) : null
  const sortedItems = compare ? [...items].sort(compare) : items
  const visibleItems = filter ? sortedItems.filter(filter) : sortedItems

  return { visibleItems } as const
}
