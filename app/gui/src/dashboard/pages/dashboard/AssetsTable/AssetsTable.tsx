/**
 * @file new AssetsTable component
 */
import { IsolateLayout } from '#/components/IsolateLayout'
import type AssetQuery from '#/utilities/AssetQuery'
import {
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table'
import { useMemo, useState, type Dispatch, type SetStateAction } from 'react'
import * as aria from 'react-aria-components'
import {
  Column,
  DEFAULT_ENABLED_COLUMNS,
  getColumnList,
  type SortableColumn,
} from '../../../components/dashboard/column/columnUtils'
import { useAssetsTableItems } from '../../../layouts/Drive/assetsTableItemsHooks'
import { useAssetTree } from '../../../layouts/Drive/assetTreeHooks'
import type { Category } from '../../../layouts/Drive/Categories/Category'
import { useDirectoryIds } from '../../../layouts/Drive/directoryIdsHooks'
import { useUser } from '../../../providers/AuthProvider'
import { useBackend } from '../../../providers/BackendProvider'
import type { AnyAsset } from '../../../services/Backend'
import type AssetTreeNode from '../../../utilities/AssetTreeNode'
import type { SortInfo } from '../../../utilities/sorting'
import { NameCell } from './Cells'
import { ModifiedDateCell } from './Cells/ModifiedDateCell'

/**
 * Props for {@link AssetsTable}
 */
export interface AssetsTableProps {
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
  readonly category: Category
}

const COLUMN_HELPER = createColumnHelper<AssetTreeNode<AnyAsset>>()

/**
 * Assets table
 */
export function AssetsTable(props: AssetsTableProps) {
  const { query, setQuery, category } = props

  const [sortInfo, setSortInfo] = useState<SortInfo<SortableColumn> | null>(null)

  const backend = useBackend(category)
  const [enabledColumns, setEnabledColumns] = useState(DEFAULT_ENABLED_COLUMNS)
  const user = useUser()

  const columns = [
    COLUMN_HELPER.accessor('item.title', {
      header: 'Name',
      cell: (info) => <NameCell state={info} />,
    }),
    COLUMN_HELPER.accessor('item.modifiedAt', {
      header: 'Modified',
      cell: (info) => <ModifiedDateCell state={info} />,
    }),
    COLUMN_HELPER.accessor('item.labels', {
      header: 'Labels',
      cell: (info) => <span>{info.getValue()}</span>,
    }),
  ]

  const hiddenColumns = useMemo(
    () =>
      getColumnList(user, backend.type, category).filter((column) => !enabledColumns.has(column)),
    [backend.type, category, enabledColumns, user],
  )

  const { rootDirectoryId, rootDirectory, expandedDirectoryIds, replaceExpandedDirectoryIds } =
    useDirectoryIds({ category })
  const { isLoading, isError, assetTree } = useAssetTree({
    hidden: false,
    category,
    rootDirectory,
    expandedDirectoryIds,
  })
  const { displayItems, visibleItems, visibilities } = useAssetsTableItems({
    assetTree,
    query,
    sortInfo,
    expandedDirectoryIds,
  })

  const tableState = useReactTable({
    _features: [],
    data: displayItems,
    getCoreRowModel: getCoreRowModel(),
    state: {
      columnVisibility: {
        [Column.name]: enabledColumns.has(Column.name),
        [Column.modified]: enabledColumns.has(Column.modified),
        [Column.sharedWith]: enabledColumns.has(Column.sharedWith),
        [Column.labels]: enabledColumns.has(Column.labels),
      },
      editingRowId: null,
    },
    columns,
    debugTable: true,
    debugHeaders: true,
    debugColumns: true,
  })

  return (
    <>
      <IsolateLayout className="h-full w-full">
        <div className="flex h-full w-full flex-col overflow-y-auto pb-36">
          <aria.Table>
            <aria.TableHeader>
              {tableState.getHeaderGroups().map((headerGroup) => {
                return headerGroup.headers.map((header, index) => (
                  <aria.Column isRowHeader={index === 0} key={header.id}>
                    {flexRender(header.column.columnDef.header, header.getContext())}
                  </aria.Column>
                ))
              })}
            </aria.TableHeader>

            <aria.TableBody items={tableState.getRowModel().rows}>
              {(row) => {
                return (
                  <aria.Row key={row.id} id={row.id}>
                    {row.getVisibleCells().map((cell) => (
                      <aria.Cell key={cell.id} id={cell.id}>
                        {flexRender(cell.column.columnDef.cell, cell.getContext())}
                      </aria.Cell>
                    ))}
                  </aria.Row>
                )
              }}
            </aria.TableBody>
          </aria.Table>
        </div>
      </IsolateLayout>
    </>
  )
}
