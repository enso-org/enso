/** @file Rows for each of a directory's children. */
import * as React from 'react'

import * as backendHooks from '#/hooks/backendHooks'

import AssetRow from '#/components/dashboard/AssetRow'
import * as columnUtils from '#/components/dashboard/column/columnUtils'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as sorting from '#/utilities/sorting'
import Visibility from '#/utilities/Visibility'

// ==================================
// === DirectoryChildrenAssetRows ===
// ==================================

/** Props for a {@link DirectoryChildrenAssetRows}. */
export interface DirectoryChildrenAssetRowsProps {
  readonly backend: Backend
  readonly depth: number
  readonly directory: backendModule.DirectoryAsset
  readonly filterBy: backendModule.FilterBy
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly filter: (asset: backendModule.AnyAsset) => boolean
}

/** Rows for each of a directory's children. */
export default function DirectoryChildrenAssetRows(props: DirectoryChildrenAssetRowsProps) {
  const { backend, depth, directory, filterBy, sortInfo, filter } = props
  const children = backendHooks.useBackendListDirectory(backend, directory.id, filterBy)
  const displayItems = React.useMemo(() => {
    if (children == null) {
      return null
    } else if (sortInfo == null) {
      return children
    } else {
      const multiplier = sortInfo.direction === sorting.SortDirection.ascending ? 1 : -1
      let compare: (a: backendModule.AnyAsset, b: backendModule.AnyAsset) => number
      switch (sortInfo.field) {
        case columnUtils.Column.name: {
          compare = (a, b) => {
            const aTitle = a.title.toLowerCase()
            const bTitle = b.title.toLowerCase()
            if (aTitle === bTitle) {
              const delta = a.title > b.title ? 1 : a.title < b.title ? -1 : 0
              return multiplier * delta
            } else {
              const delta = aTitle > bTitle ? 1 : aTitle < bTitle ? -1 : 0
              return multiplier * delta
            }
          }
          break
        }
        case columnUtils.Column.modified: {
          compare = (a, b) => {
            const aOrder = Number(new Date(a.modifiedAt))
            const bOrder = Number(new Date(b.modifiedAt))
            return multiplier * (aOrder - bOrder)
          }
          break
        }
      }
      return [...children].sort(compare)
    }
  }, [children, sortInfo])
  const visibilities = React.useMemo(() => {
    const map = new Map<backendModule.AssetId, Visibility>()
    const processNode = (node: backendModule.AnyAsset) => {
      let displayState = Visibility.hidden
      const visible = filter(node)
      for (const child of node.children ?? []) {
        if (visible && child.type === backendModule.AssetType.specialEmpty) {
          map.set(child.id, Visibility.visible)
        } else {
          processNode(child)
        }
        if (map.get(child.id) !== Visibility.hidden) {
          displayState = Visibility.faded
        }
      }
      if (visible) {
        displayState = Visibility.visible
      }
      map.set(node.id, displayState)
      return displayState
    }
    processNode(children)
    return map
  }, [children, filter])
  const visibleItems = React.useMemo(
    () => displayItems?.filter(item => visibilities.get(item.id) !== Visibility.hidden) ?? [],
    [displayItems, visibilities]
  )
  // FIXME: Filtering

  return visibleItems.map(item => <AssetRow item={item} depth={depth} />)
}
