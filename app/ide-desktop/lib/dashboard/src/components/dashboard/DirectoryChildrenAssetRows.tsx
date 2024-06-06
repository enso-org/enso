/** @file Rows for each of a directory's children. */
import * as React from 'react'

import * as backendHooks from '#/hooks/backendHooks'

import AssetRows from '#/components/dashboard/AssetRows'
import * as columnUtils from '#/components/dashboard/column/columnUtils'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as sorting from '#/utilities/sorting'

// ==================================
// === DirectoryChildrenAssetRows ===
// ==================================

/** Props for a {@link DirectoryChildrenAssetRows}. */
export interface DirectoryChildrenAssetRowsProps {
  readonly parentRef: React.RefObject<HTMLTableRowElement>
  readonly backend: Backend
  readonly depth: number
  readonly directory: backendModule.DirectoryAsset
  readonly filterBy: backendModule.FilterBy | null
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly filter: (asset: backendModule.AnyAsset) => boolean
}

/** Rows for each of a directory's children. */
export default function DirectoryChildrenAssetRows(props: DirectoryChildrenAssetRowsProps) {
  const { parentRef, backend, depth, directory, filterBy, sortInfo, filter } = props
  const children = backendHooks.useBackendListDirectory(
    backend,
    directory.id,
    ...(filterBy == null ? [] : [filterBy])
  )
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
  // FIXME: Show assets again when one of their children are present in search results.
  const visibleItems = React.useMemo(
    () => displayItems?.filter(filter) ?? [],
    [displayItems, filter]
  )

  // FIXME: AssetRowProps
  return visibleItems.map(item => <AssetRows parentRef={parentRef} item={item} depth={depth} />)
}
