/** @file An {@link AssetRow} for an asset, and its children (if present). */
import * as React from 'react'

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
  readonly hideRoot?: boolean
  readonly backend: Backend
  readonly filterBy: backendModule.FilterBy
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly filter: (asset: backendModule.AnyAsset) => boolean
}

/** An {@link AssetRow} for an asset, and its children (if present). */
export default function AssetRows(props: AssetRowsProps) {
  const { hideRoot = false, backend, filterBy, sortInfo, filter, ...assetRowProps } = props
  const { item, depth } = assetRowProps

  return (
    <>
      {!hideRoot && <AssetRow {...assetRowProps} />}
      {item.type === backendModule.AssetType.directory && (
        <DirectoryChildrenAssetRows
          backend={backend}
          depth={depth + 1}
          directory={item}
          filterBy={filterBy}
          sortInfo={sortInfo}
          filter={filter}
        />
      )}
    </>
  )
}
