/** @file A hook to return the items in the assets table. */
import type { AnyAsset, AssetId } from 'enso-common/src/services/Backend'
import { AssetType, getAssetPermissionName } from 'enso-common/src/services/Backend'
import { PermissionAction } from 'enso-common/src/utilities/permissions'

import type { SortableColumn } from '#/components/dashboard/column/columnUtils'
import { assetCompareFunction } from '#/layouts/Drive/compareAssets'
import { useText } from '#/providers/TextProvider'
import type { DirectoryId } from '#/services/ProjectManager'
import type AssetQuery from '#/utilities/AssetQuery'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import Visibility from '#/utilities/Visibility'
import { fileExtension } from '#/utilities/fileInfo'
import type { SortInfo } from '#/utilities/sorting'
import { regexEscape } from '#/utilities/string'
import { createStore, useStore } from '#/utilities/zustand.ts'
import invariant from 'tiny-invariant'

/** Options for {@link useAssetsTableItems}. */
export interface UseAssetsTableOptions {
  readonly assetTree: AnyAssetTreeNode
  readonly query: AssetQuery
  readonly sortInfo: SortInfo<SortableColumn> | null
  readonly expandedDirectoryIds: readonly DirectoryId[]
}

export const ASSET_ITEMS_STORE = createStore<{
  readonly items: AnyAsset[]
  readonly setItems: (items: AnyAsset[]) => void
}>((set) => ({
  items: [],
  setItems: (items) => {
    set({ items })
  },
}))

/**
 * Return the asset with the given id.
 */
export function useAsset(id: AssetId) {
  return useStore(
    ASSET_ITEMS_STORE,
    (store) => store.items.find((item) => item.id === id) ?? null,
    { unsafeEnableTransition: true },
  )
}

/**
 * Return the asset with the given id, or throw an error if it is undefined.
 */
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
  const { assetTree, sortInfo, query, expandedDirectoryIds } = options

  const { locale } = useText()
  const setAssetItems = useStore(ASSET_ITEMS_STORE, (store) => store.setItems)

  const filter = (() => {
    const globCache: Record<string, RegExp> = {}
    if (/^\s*$/.test(query.query)) {
      return null
    } else {
      return (node: AnyAssetTreeNode) => {
        if (
          node.item.type === AssetType.specialEmpty ||
          node.item.type === AssetType.specialLoading
        ) {
          return false
        }
        const assetType =
          node.item.type === AssetType.directory ? 'folder'
          : node.item.type === AssetType.datalink ? 'datalink'
          : String(node.item.type)
        const assetExtension =
          node.item.type !== AssetType.file ? null : fileExtension(node.item.title).toLowerCase()
        const assetModifiedAt = new Date(node.item.modifiedAt)
        const nodeLabels: readonly string[] = node.item.labels ?? []
        const lowercaseName = node.item.title.toLowerCase()
        const lowercaseDescription = node.item.description?.toLowerCase() ?? ''
        const owners =
          node.item.permissions
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

  const visibilities = (() => {
    const map = new Map<AssetId, Visibility>()

    const processNode = (node: AnyAssetTreeNode) => {
      let displayState = Visibility.hidden
      const visible = filter?.(node) ?? true

      for (const child of node.children ?? []) {
        if (visible && child.item.type === AssetType.specialEmpty) {
          map.set(child.item.id, Visibility.visible)
        } else {
          processNode(child)
        }

        if (map.get(child.item.id) !== Visibility.hidden) {
          displayState = Visibility.faded
        }
      }

      if (visible) {
        displayState = Visibility.visible
      }

      map.set(node.item.id, displayState)

      return displayState
    }

    processNode(assetTree)

    return map
  })()

  const displayItems = (() => {
    if (sortInfo == null) {
      const flatTree = assetTree.preorderTraversal((children) =>
        children.filter((child) => expandedDirectoryIds.includes(child.item.parentId)),
      )

      return flatTree
    } else {
      const compareAssets = assetCompareFunction(sortInfo, locale)
      const compare = (a: AnyAssetTreeNode, b: AnyAssetTreeNode) => compareAssets(a.item, b.item)
      const flatTree = assetTree.preorderTraversal((tree) =>
        [...tree]
          .filter((child) => expandedDirectoryIds.includes(child.item.parentId))
          .sort(compare),
      )

      return flatTree
    }
  })()

  setAssetItems(displayItems.map((item) => item.item))

  const visibleItems = displayItems.filter(
    (item) => visibilities.get(item.item.id) !== Visibility.hidden,
  )

  return { visibilities, displayItems, visibleItems } as const
}
