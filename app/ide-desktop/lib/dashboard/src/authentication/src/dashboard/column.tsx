/** @file Column types and column display modes. */
import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'
import AccessedDataIcon from 'enso-assets/accessed_data.svg'
import DocsIcon from 'enso-assets/docs.svg'
import PeopleIcon from 'enso-assets/people.svg'
import PlusIcon from 'enso-assets/plus.svg'
import TagIcon from 'enso-assets/tag.svg'
import TimeIcon from 'enso-assets/time.svg'

import * as authProvider from '../authentication/providers/auth'
import * as backend from './backend'
import * as dateTime from './dateTime'
import * as modalProvider from '../providers/modal'
import * as tableColumn from './components/tableColumn'
import * as uniqueString from '../uniqueString'

import * as assetsTable from './components/assetsTable'
import PermissionDisplay, * as permissionDisplay from './components/permissionDisplay'
import AssetNameColumn from './components/assetNameColumn'
import ManagePermissionsModal from './components/managePermissionsModal'

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

// =================
// === Constants ===
// =================

/** An immutable empty array, useful as a React prop. */
const EMPTY_ARRAY: never[] = []

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
    'text-left bg-clip-padding border-transparent border-l-2 border-r-2 first:border-l-0 last:border-r-0'
const NORMAL_COLUMN_CSS_CLASSES = `px-4 py-1 last:rounded-r-full last:w-full ${COLUMN_CSS_CLASSES}`

/** CSS classes for every column. Currently only used to set the widths. */
export const COLUMN_CSS_CLASS: Record<Column, string> = {
    [Column.name]: `min-w-60 px-1.5 py-2 rounded-l-full ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.modified]: `min-w-40 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.sharedWith]: `min-w-36 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.tags]: `min-w-80 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.accessedByProjects]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.accessedData]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
    [Column.docs]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
} as const

/** {@link table.ColumnProps} for an unknown variant of {@link backend.Asset}. */
export type AssetColumnProps<T extends backend.AnyAsset> = tableColumn.TableColumnProps<
    T,
    assetsTable.AssetsTableState,
    assetsTable.AssetRowState,
    T['id']
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
function LastModifiedColumn(props: AssetColumnProps<backend.AnyAsset>) {
    return <>{props.item.modifiedAt && dateTime.formatDateTime(new Date(props.item.modifiedAt))}</>
}

/** Props for a {@link UserPermissionDisplay}. */
interface InternalUserPermissionDisplayProps {
    user: backend.UserPermissions
    item: backend.Asset
    emailsOfUsersWithPermission: Set<backend.EmailAddress>
    ownsThisAsset: boolean
    onDelete: () => void
    onPermissionsChange: (permissions: backend.PermissionAction[]) => void
}

// =============================
// === UserPermissionDisplay ===
// =============================

/** Displays permissions for a user on a specific asset. */
function UserPermissionDisplay(props: InternalUserPermissionDisplayProps) {
    const {
        user,
        item,
        emailsOfUsersWithPermission,
        ownsThisAsset,
        onDelete,
        onPermissionsChange,
    } = props
    const { setModal } = modalProvider.useSetModal()
    const [permissions, setPermissions] = React.useState(user.permissions)
    const [oldPermissions, setOldPermissions] = React.useState(user.permissions)
    const [isHovered, setIsHovered] = React.useState(false)
    const [isDeleting, setIsDeleting] = React.useState(false)

    React.useEffect(() => {
        setPermissions(user.permissions)
    }, [user.permissions])

    return isDeleting ? null : (
        <PermissionDisplay
            key={user.user.pk}
            permissions={permissionDisplay.permissionActionsToPermissions(permissions)}
            className={ownsThisAsset ? 'cursor-pointer hover:shadow-soft hover:z-10' : ''}
            onClick={event => {
                event.stopPropagation()
                if (ownsThisAsset) {
                    setModal(
                        <ManagePermissionsModal
                            key={Number(new Date())}
                            user={user.user}
                            initialPermissions={user.permissions}
                            asset={item}
                            emailsOfUsersWithPermission={emailsOfUsersWithPermission}
                            eventTarget={event.currentTarget}
                            onSubmit={(_users, newPermissions) => {
                                if (newPermissions.length === 0) {
                                    setIsDeleting(true)
                                } else {
                                    setOldPermissions(permissions)
                                    setPermissions(newPermissions)
                                    onPermissionsChange(newPermissions)
                                }
                            }}
                            onSuccess={(_users, newPermissions) => {
                                if (newPermissions.length === 0) {
                                    onDelete()
                                }
                            }}
                            onFailure={() => {
                                setIsDeleting(false)
                                setPermissions(oldPermissions)
                                onPermissionsChange(oldPermissions)
                            }}
                        />
                    )
                }
            }}
            onMouseEnter={() => {
                setIsHovered(true)
            }}
            onMouseLeave={() => {
                setIsHovered(false)
            }}
        >
            {isHovered && (
                <div className="relative">
                    <div className="absolute text-primary bottom-2 left-1/2 -translate-x-1/2 rounded-full shadow-soft bg-white px-2 py-1">
                        {user.user.user_email}
                    </div>
                </div>
            )}
            {user.user.user_name}
        </PermissionDisplay>
    )
}

// ========================
// === SharedWithColumn ===
// ========================

/** A column listing the users with which this asset is shared. */
function SharedWithColumn(props: AssetColumnProps<backend.AnyAsset>) {
    const { item } = props
    const session = authProvider.useNonPartialUserSession()
    const { setModal } = modalProvider.useSetModal()
    const [permissions, setPermissions] = React.useState(() =>
        backend.groupPermissionsByUser(item.permissions ?? [])
    )
    const [oldPermissions, setOldPermissions] = React.useState(permissions)
    const [isHovered, setIsHovered] = React.useState(false)
    const emailsOfUsersWithPermission = React.useMemo(
        () => new Set(permissions.map(permission => permission.user.user_email)),
        [permissions]
    )
    const selfPermission = item.permissions?.find(
        permission => permission.user.user_email === session.organization?.email
    )?.permission
    const ownsThisAsset = selfPermission === backend.PermissionAction.own
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
            {permissions.map(user => (
                <UserPermissionDisplay
                    key={user.user.user_email}
                    user={user}
                    item={item}
                    emailsOfUsersWithPermission={emailsOfUsersWithPermission}
                    ownsThisAsset={ownsThisAsset}
                    onDelete={() => {
                        setPermissions(
                            permissions.filter(
                                permission => permission.user.user_email !== user.user.user_email
                            )
                        )
                    }}
                    onPermissionsChange={newPermissions => {
                        setPermissions(
                            permissions.map(permission =>
                                permission.user.user_email === user.user.user_email
                                    ? { user: user.user, permissions: newPermissions }
                                    : permission
                            )
                        )
                    }}
                />
            ))}
            {ownsThisAsset && isHovered && (
                <button
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <ManagePermissionsModal
                                key={uniqueString.uniqueString()}
                                asset={item}
                                initialPermissions={EMPTY_ARRAY}
                                emailsOfUsersWithPermission={emailsOfUsersWithPermission}
                                eventTarget={event.currentTarget}
                                onSubmit={(users, newPermissions) => {
                                    setOldPermissions(permissions)
                                    setPermissions([
                                        ...permissions,
                                        ...users.map(user => {
                                            const userPermissions: backend.UserPermissions = {
                                                user: {
                                                    pk: user.id,
                                                    // The names come from a third-party API
                                                    // and cannot be changed.
                                                    /* eslint-disable @typescript-eslint/naming-convention */
                                                    user_name: user.name,
                                                    user_email: user.email,
                                                    /** {@link SharedWithColumn} is only accessible
                                                     * if `session.organization` is not `null`. */
                                                    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                                                    organization_id: session.organization!.id,
                                                    /* eslint-enable @typescript-eslint/naming-convention */
                                                },
                                                permissions: newPermissions,
                                            }
                                            return userPermissions
                                        }),
                                    ])
                                }}
                                onFailure={() => {
                                    setPermissions(oldPermissions)
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

export const COLUMN_HEADING: Record<
    Column,
    (props: tableColumn.TableColumnHeadingProps<assetsTable.AssetsTableState>) => JSX.Element
> = {
    [Column.name]: () => <>{COLUMN_NAME[Column.name]}</>,
    [Column.modified]: () => (
        <div className="flex gap-2">
            <img src={TimeIcon} /> {COLUMN_NAME[Column.modified]}
        </div>
    ),
    [Column.sharedWith]: () => (
        <div className="flex gap-2">
            <img src={PeopleIcon} /> {COLUMN_NAME[Column.sharedWith]}
        </div>
    ),
    [Column.tags]: () => (
        <div className="flex gap-2">
            <img src={TagIcon} /> {COLUMN_NAME[Column.tags]}
        </div>
    ),
    [Column.accessedByProjects]: () => (
        <div className="flex gap-2">
            <img src={AccessedByProjectsIcon} /> {COLUMN_NAME[Column.accessedByProjects]}
        </div>
    ),
    [Column.accessedData]: () => (
        <div className="flex gap-2">
            <img src={AccessedDataIcon} /> {COLUMN_NAME[Column.accessedData]}
        </div>
    ),
    [Column.docs]: () => (
        <div className="flex gap-2">
            <img src={DocsIcon} /> {COLUMN_NAME[Column.docs]}
        </div>
    ),
}

/** React components for every column except for the name column. */
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
export const COLUMN_RENDERER: Record<
    Column,
    (props: AssetColumnProps<backend.AnyAsset>) => JSX.Element
> = {
    [Column.name]: AssetNameColumn,
    [Column.modified]: LastModifiedColumn,
    [Column.sharedWith]: SharedWithColumn,
    [Column.tags]: PlaceholderColumn,
    [Column.accessedByProjects]: PlaceholderColumn,
    [Column.accessedData]: PlaceholderColumn,
    [Column.docs]: PlaceholderColumn,
}
