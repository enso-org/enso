/** @file Column types and column display modes. */
import * as React from 'react'

import * as backend from './backend'
import * as modalProvider from '../providers/modal'
import * as rows from './components/rows'

import PermissionDisplay, * as permissionDisplay from './components/permissionDisplay'
import ContextMenu from './components/contextMenu'
import ContextMenuEntry from './components/contextMenuEntry'

// =============
// === Types ===
// =============

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
    lastModified = 'last-modified',
    sharedWith = 'shared-with',
    docs = 'docs',
    labels = 'labels',
    dataAccess = 'data-access',
    usagePlan = 'usage-plan',
    engine = 'engine',
    ide = 'ide',
}

// =================
// === Constants ===
// =================

/** English names for every column except for the name column. */
export const COLUMN_NAME: Record<Exclude<Column, Column.name>, string> = {
    [Column.lastModified]: 'Last modified',
    [Column.sharedWith]: 'Shared with',
    [Column.docs]: 'Docs',
    [Column.labels]: 'Labels',
    [Column.dataAccess]: 'Data access',
    [Column.usagePlan]: 'Usage plan',
    [Column.engine]: 'Engine',
    [Column.ide]: 'IDE',
} as const

/** A list of column display modes and names, in order. */
export const COLUMN_DISPLAY_MODES_AND_NAMES: [ColumnDisplayMode, string][] = [
    [ColumnDisplayMode.all, 'All'],
    [ColumnDisplayMode.compact, 'Compact'],
    [ColumnDisplayMode.docs, 'Docs'],
    [ColumnDisplayMode.settings, 'Settings'],
]

/** The list of columns displayed on each `ColumnDisplayMode`. */
export const COLUMNS_FOR: Record<ColumnDisplayMode, Column[]> = {
    [ColumnDisplayMode.release]: [Column.name, Column.lastModified, Column.sharedWith],
    [ColumnDisplayMode.all]: [
        Column.name,
        Column.lastModified,
        Column.sharedWith,
        Column.labels,
        Column.dataAccess,
        Column.usagePlan,
        Column.engine,
        Column.ide,
    ],
    [ColumnDisplayMode.compact]: [
        Column.name,
        Column.lastModified,
        Column.sharedWith,
        Column.labels,
        Column.dataAccess,
    ],
    [ColumnDisplayMode.docs]: [Column.name, Column.lastModified, Column.docs],
    [ColumnDisplayMode.settings]: [
        Column.name,
        Column.lastModified,
        Column.usagePlan,
        Column.engine,
        Column.ide,
    ],
}

/** The corresponding `Permissions` for each backend `PermissionAction`. */
const PERMISSION: Record<backend.PermissionAction, permissionDisplay.Permissions> = {
    [backend.PermissionAction.own]: { type: permissionDisplay.Permission.owner },
    [backend.PermissionAction.execute]: {
        type: permissionDisplay.Permission.regular,
        read: false,
        write: false,
        docsWrite: false,
        exec: true,
    },
    [backend.PermissionAction.edit]: {
        type: permissionDisplay.Permission.regular,
        read: false,
        write: true,
        docsWrite: false,
        exec: false,
    },
    [backend.PermissionAction.read]: {
        type: permissionDisplay.Permission.regular,
        read: true,
        write: false,
        docsWrite: false,
        exec: false,
    },
}

/** React components for every column except for the name column. */
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
export const COLUMN_RENDERER: Record<
    Exclude<Column, Column.name>,
    (props: rows.ColumnProps<backend.Asset, unknown>) => JSX.Element
> = {
    [Column.lastModified]: () => <></>,
    [Column.sharedWith]: props => (
        <>
            {(props.item.permissions ?? []).map(user => (
                <PermissionDisplay
                    key={user.user.organization_id}
                    permissions={PERMISSION[user.permission]}
                >
                    <img
                        className="rounded-full h-6"
                        src="https://faces-img.xcdn.link/image-lorem-face-4742.jpg"
                    />
                </PermissionDisplay>
            ))}
        </>
    ),
    [Column.docs]: () => <></>,
    [Column.labels]: () => {
        const { setModal } = modalProvider.useSetModal()

        // This is not a React component even though it contains JSX.
        // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
        const onContextMenu = (event: React.MouseEvent) => {
            event.preventDefault()
            event.stopPropagation()
            setModal(() => (
                <ContextMenu event={event}>
                    <ContextMenuEntry
                        disabled
                        onClick={() => {
                            // TODO: Wait for backend implementation.
                        }}
                    >
                        Rename label
                    </ContextMenuEntry>
                </ContextMenu>
            ))
        }
        return <></>
    },
    [Column.dataAccess]: () => <></>,
    [Column.usagePlan]: () => <></>,
    [Column.engine]: () => <></>,
    [Column.ide]: () => <></>,
}
