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
  readonly onChildRendered: () => void
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
    onChildRendered,
    ...assetRowProps
  } = props
  const { item, depth, columns, state } = assetRowProps
  const rootRef = React.useRef<HTMLTableRowElement>(null)
  const isOpen = store.useStore(
    storeState => storeState.getAssetState(backend.type, item.id).isOpen
  )
  const [areChildrenVisible, setAreChildrenVisible] = React.useState(false)
  const isVisible = React.useMemo(() => !hideRoot && filter(item), [hideRoot, filter, item])

  React.useEffect(() => {
    setAreChildrenVisible(false)
  }, [filter])

  const row = () => {
    setTimeout(() => {
      onChildRendered()
    })
    return (
      <AssetRow isDisabled={!isVisible} parentRef={parentRef} ref={rootRef} {...assetRowProps} />
    )
  }

  if (item.type !== backendModule.AssetType.directory || !isOpen) {
    return isVisible && row()
  } else {
    const children = (
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
        onChildRendered={() => {
          // Skip the unnecessary extra re-render if the row is already visible.
          if (isVisible) {
            setAreChildrenVisible(true)
          }
        }}
      />
    )
    // By this point, `didRowRenderRef` will have been set by the children already.
    return (
      <>
        {!hideRoot && (isVisible || areChildrenVisible) && row()}
        {children}
      </>
    )
  }
}
