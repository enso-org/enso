/** @file Column types and column display modes. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'
import PlusIcon from 'enso-assets/plus.svg'

import * as authProvider from '../authentication/providers/auth'
import * as backend from './backend'
import * as dateTime from './dateTime'
import * as modalProvider from '../providers/modal'
import * as tableColumn from './components/tableColumn'

import PermissionDisplay, * as permissionDisplay from './components/permissionDisplay'
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
export type ExtraColumn =
    | Column.accessedByProjects
    | Column.accessedData
    | Column.docs
    | Column.tags

// =================
// === Constants ===
// =================

/** An immutable empty array, useful as a React prop. */
const EMPTY_ARRAY: never[] = []

/** English names for every column except for the name column. */
export const COLUMN_NAME: Record<Exclude<Column, Column.name>, string> = {
    [Column.modified]: 'Modified',
    [Column.sharedWith]: 'Shared with',
    [Column.tags]: 'Tags',
    [Column.accessedByProjects]: 'Accessed by projects',
    [Column.accessedData]: 'Accessed data',
    [Column.docs]: 'Docs',
} as const

/** CSS classes for every column. Currently only used to set the widths. */
export const COLUMN_CSS_CLASS: Record<Column, string> = {
    [Column.name]: 'w-60',
    [Column.modified]: 'w-40',
    [Column.sharedWith]: 'w-36',
    [Column.tags]: 'w-80',
    [Column.accessedByProjects]: 'w-96',
    [Column.accessedData]: 'w-96',
    [Column.docs]: 'w-96',
} as const

/** A list of column display modes and names, in order. */
export const COLUMN_DISPLAY_MODES_AND_NAMES: [ColumnDisplayMode, string][] = [
    [ColumnDisplayMode.all, 'All'],
    [ColumnDisplayMode.compact, 'Compact'],
    [ColumnDisplayMode.docs, 'Docs'],
    [ColumnDisplayMode.settings, 'Settings'],
]

/** {@link table.ColumnProps} for an unknown variant of {@link backend.Asset}. */
type AnyAssetColumnProps = Omit<
    tableColumn.TableColumnProps<backend.Asset>,
    'rowState' | 'setItem' | 'setRowState' | 'state'
>

/** A column displaying the time at which the asset was last modified. */
function LastModifiedColumn(props: AnyAssetColumnProps) {
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
            className={`border-2 rounded-full -ml-5 first:ml-0 ${
                ownsThisAsset ? 'cursor-pointer hover:shadow-soft hover:z-10' : ''
            }`}
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
                    <div className="absolute bottom-2 left-1/2 -translate-x-1/2 rounded-full shadow-soft bg-white px-2 py-1">
                        {user.user.user_email}
                    </div>
                </div>
            )}
            <img src={DefaultUserIcon} height={24} width={24} />
        </PermissionDisplay>
    )
}

/** A column listing the users with which this asset is shared. */
function SharedWithColumn(props: AnyAssetColumnProps) {
    const { item } = props
    const session = authProvider.useNonPartialUserSession()
    const { setModal } = modalProvider.useSetModal()
    const [permissions, setPermissions] = React.useState(() =>
        backend.groupPermissionsByUser(item.permissions ?? [])
    )
    const [oldPermissions, setOldPermissions] = React.useState(permissions)
    const emailsOfUsersWithPermission = React.useMemo(
        () => new Set(permissions.map(permission => permission.user.user_email)),
        [permissions]
    )
    const selfPermission = item.permissions?.find(
        permission => permission.user.user_email === session.organization?.email
    )?.permission
    const ownsThisAsset = selfPermission === backend.PermissionAction.own
    return (
        <div className="flex">
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
            {ownsThisAsset && (
                <button
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <ManagePermissionsModal
                                key={Number(new Date())}
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

/** A placeholder component for columns which do not yet have corresponding data to display. */
function PlaceholderColumn() {
    return <></>
}

/** React components for every column except for the name column. */
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
export const COLUMN_RENDERER: Record<
    Exclude<Column, Column.name>,
    (props: AnyAssetColumnProps) => JSX.Element
> = {
    [Column.modified]: LastModifiedColumn,
    [Column.sharedWith]: SharedWithColumn,
    [Column.tags]: PlaceholderColumn,
    [Column.accessedByProjects]: PlaceholderColumn,
    [Column.accessedData]: PlaceholderColumn,
    [Column.docs]: PlaceholderColumn,
}
