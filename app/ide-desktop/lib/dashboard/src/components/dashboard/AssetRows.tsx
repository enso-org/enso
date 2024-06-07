/** @file An {@link AssetRow} for an asset, and its children (if present). */
import * as React from 'react'

import * as store from '#/store'

import type * as assetRow from '#/components/dashboard/AssetRow'
import AssetRow from '#/components/dashboard/AssetRow'
import type * as columnUtils from '#/components/dashboard/column/columnUtils'
import DirectoryChildrenAssetRows from '#/components/dashboard/DirectoryChildrenAssetRows'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import type * as sorting from '#/utilities/sorting'

// =================
// === AssetRows ===
// =================

/** Props for a {@link AssetRows}. */
export interface AssetRowsProps extends assetRow.AssetRowProps {
  readonly parentRef: React.RefObject<HTMLTableRowElement> | null
  readonly hideRoot?: boolean
  readonly backend: Backend
  readonly filterBy: backendModule.FilterBy | null
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly filter: (asset: backendModule.AnyAsset) => boolean
}

/** An {@link AssetRow} for an asset, and its children (if present). */
export default function AssetRows(props: AssetRowsProps) {
  const {
    parentRef,
    hideRoot = false,
    backend,
    filterBy,
    sortInfo,
    filter,
    ...assetRowProps
  } = props
  const { item, depth, columns, state } = assetRowProps
  const rootRef = React.useRef<HTMLTableRowElement>(null)
  const isOpen = store.useStore(
    storeState => storeState.getAssetState(backend.type, item.id).isOpen
  )

  const row = !hideRoot && <AssetRow parentRef={parentRef} ref={rootRef} {...assetRowProps} />

  return item.type !== backendModule.AssetType.directory ? (
    row
  ) : (
    <>
      {row}
      {isOpen && (
        <DirectoryChildrenAssetRows
          parentRef={rootRef}
          backend={backend}
          depth={depth + 1}
          directory={item}
          filterBy={filterBy}
          sortInfo={sortInfo}
          filter={filter}
          columns={columns}
          state={state}
        />
      )}
    </>
  )
}
