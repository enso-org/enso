/** @file Column types and column display modes. */
import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'
import AccessedDataIcon from 'enso-assets/accessed_data.svg'
import DocsIcon from 'enso-assets/docs.svg'
import PeopleIcon from 'enso-assets/people.svg'
import PlusIcon from 'enso-assets/plus.svg'
import SortAscendingIcon from 'enso-assets/sort_ascending.svg'
import SortDescendingIcon from 'enso-assets/sort_descending.svg'
import TagIcon from 'enso-assets/tag.svg'
import TimeIcon from 'enso-assets/time.svg'

import * as assetEvent from './events/assetEvent'
import * as assetTreeNode from './assetTreeNode'
import * as authProvider from '../authentication/providers/auth'
import * as backend from './backend'
import * as dateTime from './dateTime'
import * as modalProvider from '../providers/modal'
import * as sorting from './sorting'
import * as tableColumn from './components/tableColumn'
import * as uniqueString from '../uniqueString'

import * as assetsTable from './components/assetsTable'
import AssetNameColumn from './components/assetNameColumn'
import ManagePermissionsModal from './components/managePermissionsModal'
import PermissionDisplay from './components/permissionDisplay'
import SvgMask from '../authentication/components/svgMask'

// =============
// === Types ===
// =============

/** Determines which columns are visible. */
export enum ColumnDisplayMode {
    /** Show only columns which are ready for release. */
    release = 'release',
    /** Show all columns. */
    all = 'all',
    /** Show only name and metadata. */
    compact = 'compact',
    /** Show only columns relevant to documentation editors. */
    docs = 'docs',
    /** Show only name, metadata, and configuration options. */
    settings = 'settings',
}

/** Column type. */
export enum Column {
    name = 'name',
    modified = 'modified',
    sharedWith = 'shared-with',
    tags = 'tags',
    accessedByProjects = 'accessed-by-projects',
    accessedData = 'accessed-data',
    docs = 'docs',
}

/** Columns that can be toggled between visible and hidden. */
export type ExtraColumn = (typeof EXTRA_COLUMNS)[number]

/** Columns that can be used as a sort column. */
export type SortableColumn = Column.modified | Column.name

// =================
// === Constants ===
// =================

/** The list of extra columns, in order. */
// This MUST be `as const`, to generate the `ExtraColumn` type above.
// eslint-disable-next-line no-restricted-syntax
export const EXTRA_COLUMNS = [
    Column.tags,
    Column.accessedByProjects,
    Column.accessedData,
    Column.docs,
] as const

export const EXTRA_COLUMN_IMAGES: Record<ExtraColumn, string> = {
    [Column.tags]: TagIcon,
    [Column.accessedByProjects]: AccessedByProjectsIcon,
    [Column.accessedData]: AccessedDataIcon,
    [Column.docs]: DocsIcon,
}

/** English names for every column except for the name column. */
export const COLUMN_NAME: Record<Column, string> = {
    [Column.name]: 'Name',
    [Column.modified]: 'Modified',
    [Column.sharedWith]: 'Shared with',
    [Column.tags]: 'Tags',
    [Column.accessedByProjects]: 'Accessed by projects',
    [Column.accessedData]: 'Accessed data',
    [Column.docs]: 'Docs',
} as const

const COLUMN_CSS_CLASSES =
    'text-left bg-clip-padding border-transparent border-l-2 border-r-2 first:border-l-0 last:border-r-0 pt-1 pb-1.5'
const NORMAL_COLUMN_CSS_CLASSES = `px-4 last:rounded-r-full last:w-full ${COLUMN_CSS_CLASSES}`

/** CSS classes for every column. Currently only used to set the widths. */
export const COLUMN_CSS_CLASS: Record<Column, string> = {
    [Column.name]: `min-w-60 px-1.5 rounded-l-full ${COLUMN_CSS_CLASSES}`,
    [Column.modified]: `min-w-40 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.sharedWith]: `min-w-36 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.tags]: `min-w-80 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.accessedByProjects]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.accessedData]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.docs]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
} as const

/** {@link table.ColumnProps} for an unknown variant of {@link backend.Asset}. */
export type AssetColumnProps = tableColumn.TableColumnProps<
    assetTreeNode.AssetTreeNode,
    assetsTable.AssetsTableState,
    assetsTable.AssetRowState,
    backend.AssetId
>

// =====================
// === getColumnList ===
// =====================

/** Return the full list of columns given the relevant current state. */
export function getColumnList(backendType: backend.BackendType, extraColumns: Set<ExtraColumn>) {
    switch (backendType) {
        case backend.BackendType.local: {
            return [Column.name, Column.modified]
        }
        case backend.BackendType.remote: {
            return [
                Column.name,
                Column.modified,
                Column.sharedWith,
                ...EXTRA_COLUMNS.filter(column => extraColumns.has(column)),
            ]
        }
    }
}

// ==========================
// === LastModifiedColumn ===
// ==========================

/** A column displaying the time at which the asset was last modified. */
function LastModifiedColumn(props: AssetColumnProps) {
    return <>{dateTime.formatDateTime(new Date(props.item.item.modifiedAt))}</>
}

/** Props for a {@link UserPermissionDisplay}. */
interface InternalUserPermissionDisplayProps {
    user: backend.UserPermission
}

// =============================
// === UserPermissionDisplay ===
// =============================

/** Displays permissions for a user on a specific asset. */
function UserPermissionDisplay(props: InternalUserPermissionDisplayProps) {
    const { user } = props
    const [permissions, setPermissions] = React.useState(user.permission)

    React.useEffect(() => {
        setPermissions(user.permission)
    }, [user.permission])

    return (
        <PermissionDisplay key={user.user.pk} action={permissions}>
            {user.user.user_name}
        </PermissionDisplay>
    )
}

// ========================
// === SharedWithColumn ===
// ========================

/** A column listing the users with which this asset is shared. */
function SharedWithColumn(props: AssetColumnProps) {
    const {
        item: { item },
        setItem,
        state: { dispatchAssetEvent },
    } = props
    const session = authProvider.useNonPartialUserSession()
    const { setModal } = modalProvider.useSetModal()
    const [isHovered, setIsHovered] = React.useState(false)
    const self = item.permissions?.find(
        permission => permission.user.user_email === session.organization?.email
    )
    const managesThisAsset =
        self?.permission === backend.PermissionAction.own ||
        self?.permission === backend.PermissionAction.admin
    const setAsset = React.useCallback(
        (valueOrUpdater: React.SetStateAction<backend.AnyAsset>) => {
            if (typeof valueOrUpdater === 'function') {
                setItem(oldItem => ({
                    ...oldItem,
                    item: valueOrUpdater(oldItem.item),
                }))
            } else {
                setItem(oldItem => ({ ...oldItem, item: valueOrUpdater }))
            }
        },
        [/* should never change */ setItem]
    )
    return (
        <div
            className="flex items-center gap-1"
            onMouseEnter={() => {
                setIsHovered(true)
            }}
            onMouseLeave={() => {
                setIsHovered(false)
            }}
        >
            {(item.permissions ?? []).map(user => (
                <UserPermissionDisplay key={user.user.user_email} user={user} />
            ))}
            {managesThisAsset && isHovered && (
                <button
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <ManagePermissionsModal
                                key={uniqueString.uniqueString()}
                                item={item}
                                setItem={setAsset}
                                self={self}
                                eventTarget={event.currentTarget}
                                doRemoveSelf={() => {
                                    dispatchAssetEvent({
                                        type: assetEvent.AssetEventType.removeSelf,
                                        id: item.id,
                                    })
                                }}
                            />
                        )
                    }}
                >
                    <img src={PlusIcon} />
                </button>
            )}
        </div>
    )
}

// =========================
// === PlaceholderColumn ===
// =========================

/** A placeholder component for columns which do not yet have corresponding data to display. */
function PlaceholderColumn() {
    return <></>
}

// =================
// === Constants ===
// =================

/** The corresponding icon URL for each {@link sorting.SortDirection}. */
const SORT_ICON: Record<sorting.SortDirection, string> = {
    [sorting.SortDirection.ascending]: SortAscendingIcon,
    [sorting.SortDirection.descending]: SortDescendingIcon,
}

export const COLUMN_HEADING: Record<
    Column,
    (props: tableColumn.TableColumnHeadingProps<assetsTable.AssetsTableState>) => JSX.Element
> = {
    [Column.name]: ({ state: { sortColumn, setSortColumn, sortDirection, setSortDirection } }) => {
        const [isHovered, setIsHovered] = React.useState(false)
        const isSortActive = sortColumn === Column.name && sortDirection != null
        return (
            <div
                className="flex items-center cursor-pointer gap-2"
                onMouseEnter={() => {
                    setIsHovered(true)
                }}
                onMouseLeave={() => {
                    setIsHovered(false)
                }}
                onClick={() => {
                    if (sortColumn === Column.name) {
                        setSortDirection(sorting.NEXT_SORT_DIRECTION[sortDirection ?? 'null'])
                    } else {
                        setSortColumn(Column.name)
                        setSortDirection(sorting.SortDirection.ascending)
                    }
                }}
            >
                {COLUMN_NAME[Column.name]}
                <img
                    src={isSortActive ? SORT_ICON[sortDirection] : SortAscendingIcon}
                    className={isSortActive ? '' : isHovered ? 'opacity-50' : 'opacity-0'}
                />
            </div>
        )
    },
    [Column.modified]: ({
        state: { sortColumn, setSortColumn, sortDirection, setSortDirection },
    }) => {
        const [isHovered, setIsHovered] = React.useState(false)
        const isSortActive = sortColumn === Column.modified && sortDirection != null
        return (
            <div
                className="flex items-center cursor-pointer gap-2"
                onMouseEnter={() => {
                    setIsHovered(true)
                }}
                onMouseLeave={() => {
                    setIsHovered(false)
                }}
                onClick={() => {
                    if (sortColumn === Column.modified) {
                        setSortDirection(sorting.NEXT_SORT_DIRECTION[sortDirection ?? 'null'])
                    } else {
                        setSortColumn(Column.modified)
                        setSortDirection(sorting.SortDirection.ascending)
                    }
                }}
            >
                <SvgMask src={TimeIcon} /> {COLUMN_NAME[Column.modified]}
                <img
                    src={isSortActive ? SORT_ICON[sortDirection] : SortAscendingIcon}
                    className={isSortActive ? '' : isHovered ? 'opacity-50' : 'opacity-0'}
                />
            </div>
        )
    },
    [Column.sharedWith]: () => (
        <div className="flex items-center gap-2">
            <SvgMask src={PeopleIcon} /> {COLUMN_NAME[Column.sharedWith]}
        </div>
    ),
    [Column.tags]: () => (
        <div className="flex items-center gap-2">
            <SvgMask src={TagIcon} /> {COLUMN_NAME[Column.tags]}
        </div>
    ),
    [Column.accessedByProjects]: () => (
        <div className="flex items-center gap-2">
            <SvgMask src={AccessedByProjectsIcon} /> {COLUMN_NAME[Column.accessedByProjects]}
        </div>
    ),
    [Column.accessedData]: () => (
        <div className="flex items-center gap-2">
            <SvgMask src={AccessedDataIcon} /> {COLUMN_NAME[Column.accessedData]}
        </div>
    ),
    [Column.docs]: () => (
        <div className="flex items-center gap-2">
            <SvgMask src={DocsIcon} /> {COLUMN_NAME[Column.docs]}
        </div>
    ),
}

/** React components for every column except for the name column. */
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
export const COLUMN_RENDERER: Record<Column, (props: AssetColumnProps) => JSX.Element> = {
    [Column.name]: AssetNameColumn,
    [Column.modified]: LastModifiedColumn,
    [Column.sharedWith]: SharedWithColumn,
    [Column.tags]: PlaceholderColumn,
    [Column.accessedByProjects]: PlaceholderColumn,
    [Column.accessedData]: PlaceholderColumn,
    [Column.docs]: PlaceholderColumn,
}
