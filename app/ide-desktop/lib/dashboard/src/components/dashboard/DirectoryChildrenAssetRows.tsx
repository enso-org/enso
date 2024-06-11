/** @file Rows for each of a directory's children. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import BlankIcon from 'enso-assets/blank.svg'

import * as backendHooks from '#/hooks/backendHooks'

import * as textProvider from '#/providers/TextProvider'

import type * as assetsTable from '#/layouts/AssetsTable'

import * as aria from '#/components/aria'
import AssetRows from '#/components/dashboard/AssetRows'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as indent from '#/utilities/indent'
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
  readonly columns: readonly columnUtils.Column[]
  readonly state: assetsTable.AssetsTableState
  readonly filter: (asset: backendModule.AnyAsset) => boolean
  readonly onChildRendered: () => void
}

/** Rows for each of a directory's children. */
export default function DirectoryChildrenAssetRows(props: DirectoryChildrenAssetRowsProps) {
  const { backend, depth, directory, filterBy, sortInfo, columns } = props
  const { getText } = textProvider.useText()
  const childAssets = backendHooks.useBackendListDirectory(
    backend,
    directory.id,
    ...(filterBy == null ? [] : [filterBy])
  )
  const displayAssets = React.useMemo(() => {
    if (childAssets == null) {
      return null
    } else if (sortInfo == null) {
      return childAssets
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
      return [...childAssets].sort(compare)
    }
  }, [childAssets, sortInfo])

  return childAssets == null ? (
    <tr>
      <td colSpan={columns.length} className="border-r p-0 rounded-rows-skip-level">
        <div
          className={tailwindMerge.twMerge(
            'flex h-row w-container justify-center rounded-full rounded-rows-child',
            indent.indentClass(depth)
          )}
        >
          <StatelessSpinner size={24} state={statelessSpinner.SpinnerState.loadingMedium} />
        </div>
      </td>
    </tr>
  ) : childAssets.length === 0 ? (
    <tr>
      <td colSpan={columns.length} className="border-r p-0 rounded-rows-skip-level">
        <div
          className={tailwindMerge.twMerge(
            'flex h-row items-center rounded-full rounded-rows-child',
            indent.indentClass(depth)
          )}
        >
          <img src={BlankIcon} />
          <aria.Text className="px-name-column-x placeholder">
            {getText('thisFolderIsEmpty')}
          </aria.Text>
        </div>
      </td>
    </tr>
  ) : (
    (displayAssets ?? []).map(item => <AssetRows key={item.id} {...props} item={item} />)
  )
}
