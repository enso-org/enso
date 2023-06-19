/** @file Column types and column display modes. */
import * as React from 'react'

import * as backend from './backend'
import * as dateTime from './dateTime'
import * as svg from '../components/svg'
import * as table from './components/table'

import PermissionDisplay, * as permissionDisplay from './components/permissionDisplay'

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

/** CSS classes for every column. Currently only used to set the widths. */
export const COLUMN_CSS_CLASS: Record<Column, string> = {
    [Column.name]: 'w-60',
    [Column.lastModified]: 'w-40',
    [Column.sharedWith]: 'w-36',
    [Column.docs]: 'w-96',
    [Column.labels]: 'w-80',
    [Column.dataAccess]: 'w-96',
    [Column.usagePlan]: 'w-40',
    [Column.engine]: 'w-20',
    [Column.ide]: 'w-20',
} as const

/** A list of column display modes and names, in order. */
export const COLUMN_DISPLAY_MODES_AND_NAMES: [ColumnDisplayMode, string][] = [
    [ColumnDisplayMode.all, 'All'],
    [ColumnDisplayMode.compact, 'Compact'],
    [ColumnDisplayMode.docs, 'Docs'],
    [ColumnDisplayMode.settings, 'Settings'],
]

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

/** {@link table.ColumnProps} for an unknown variant of {@link backend.Asset}. */
type AnyAssetColumnProps = Omit<
    table.ColumnProps<backend.Asset>,
    'rowState' | 'setRowState' | 'state'
>

/** A column displaying the time at which the asset was last modified. */
export function LastModifiedColumn(props: AnyAssetColumnProps) {
    return <>{props.item.modifiedAt && dateTime.formatDateTime(new Date(props.item.modifiedAt))}</>
}

/** A column listing the users with which this asset is shared. */
export function SharedWithColumn(props: AnyAssetColumnProps) {
    return (
        <>
            {(props.item.permissions ?? []).map(user => (
                <PermissionDisplay
                    key={user.user.organization_id}
                    permissions={PERMISSION[user.permission]}
                >
                    {svg.DEFAULT_USER_ICON}
                </PermissionDisplay>
            ))}
        </>
    )
}

/** A placeholder component for columns which do not yet have corresponding data to display. */
export function PlaceholderColumn() {
    return <></>
}

/** React components for every column except for the name column. */
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
export const COLUMN_RENDERER: Record<
    Exclude<Column, Column.name>,
    (
        props: Omit<table.ColumnProps<backend.Asset>, 'rowState' | 'setRowState' | 'state'>
    ) => JSX.Element
> = {
    [Column.lastModified]: LastModifiedColumn,
    [Column.sharedWith]: SharedWithColumn,
    [Column.docs]: PlaceholderColumn,
    [Column.labels]: PlaceholderColumn,
    [Column.dataAccess]: PlaceholderColumn,
    [Column.usagePlan]: PlaceholderColumn,
    [Column.engine]: PlaceholderColumn,
    [Column.ide]: PlaceholderColumn,
}

// ========================
// === Helper functions ===
// ========================

/** The list of columns displayed on each `ColumnDisplayMode`. */
const COLUMNS_FOR: Record<ColumnDisplayMode, Column[]> = {
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

/** Returns the list of columns to be displayed. */
export function columnsFor(displayMode: ColumnDisplayMode, backendType: backend.BackendType) {
    const columns = COLUMNS_FOR[displayMode]
    return backendType === backend.BackendType.local
        ? columns.filter(column => column !== Column.sharedWith)
        : columns
}
