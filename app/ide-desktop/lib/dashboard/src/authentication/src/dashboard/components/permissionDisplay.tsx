/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import * as backend from '../backend'

// =============
// === Types ===
// =============

/** Type of permission. This determines what kind of border is displayed. */
export enum Permission {
    owner = 'owner',
    admin = 'admin',
    edit = 'edit',
    read = 'read',
    view = 'view',
}

/** Properties common to all permissions. */
interface BasePermissions<T extends Permission> {
    type: T
}

/** Owner permissions for an asset. */
interface OwnerPermissions extends BasePermissions<Permission.owner> {}

/** Admin permissions for an asset. */
interface AdminPermissions extends BasePermissions<Permission.admin> {}

/** Editor permissions for an asset. */
interface EditPermissions extends BasePermissions<Permission.edit> {}

/** Reader permissions for an asset. */
interface ReadPermissions extends BasePermissions<Permission.read> {
    execute: boolean
    docs: boolean
}

/** Viewer permissions for an asset. */
interface ViewPermissions extends BasePermissions<Permission.view> {
    execute: boolean
    docs: boolean
}

/** Detailed permission information. This is used to draw the border. */
export type Permissions =
    | AdminPermissions
    | EditPermissions
    | OwnerPermissions
    | ReadPermissions
    | ViewPermissions

// =================
// === Constants ===
// =================

/** CSS classes for each permission. */
export const PERMISSION_CLASS_NAME: Record<Permission, string> = {
    [Permission.owner]: 'text-tag-text bg-permission-owner',
    [Permission.admin]: 'text-tag-text bg-permission-admin',
    [Permission.edit]: 'text-tag-text bg-permission-edit',
    [Permission.read]: 'text-tag-text bg-permission-read',
    [Permission.view]: 'text-tag-text-2 bg-permission-view',
} as const

/** Precedences for each permission. A lower number means a higher priority. */
const PERMISSION_PRECEDENCE: Record<Permission, number> = {
    // These are not magic numbers - they are just a sequence of numbers.
    /* eslint-disable @typescript-eslint/no-magic-numbers */
    [Permission.owner]: 0,
    [Permission.admin]: 1,
    [Permission.edit]: 2,
    [Permission.read]: 3,
    [Permission.view]: 4,
    /* eslint-enable @typescript-eslint/no-magic-numbers */
}

/** The corresponding `Permissions` for each backend `PermissionAction`. */
export const PERMISSION: Record<backend.PermissionAction, Permissions> = {
    [backend.PermissionAction.own]: { type: Permission.owner },
    [backend.PermissionAction.execute]: {
        type: Permission.read,
        execute: true,
        docs: false,
    },
    [backend.PermissionAction.edit]: { type: Permission.edit },
    [backend.PermissionAction.view]: { type: Permission.view, execute: false, docs: false },
}

// ======================
// === permissionsToX ===
// ======================

/** Converts an array of {@link backend.PermissionAction} to a {@link Permissions}. */
export function permissionActionsToPermissions(
    permissions: backend.PermissionAction[]
): Permissions {
    return permissions.reduce<Permissions>(
        (result, action) => {
            const actionResult = PERMISSION[action]
            return PERMISSION_PRECEDENCE[actionResult.type] <= PERMISSION_PRECEDENCE[result.type]
                ? actionResult
                : result
        },
        { type: Permission.view, execute: false, docs: false }
    )
}

// =================
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps extends React.PropsWithChildren {
    permissions: Permissions
    className?: string
    onClick?: React.MouseEventHandler<HTMLDivElement>
    onMouseEnter?: React.MouseEventHandler<HTMLDivElement>
    onMouseLeave?: React.MouseEventHandler<HTMLDivElement>
}

/** Colored border around icons and text indicating permissions. */
export default function PermissionDisplay(props: PermissionDisplayProps) {
    const { permissions, className, onClick, onMouseEnter, onMouseLeave, children } = props

    switch (permissions.type) {
        case Permission.owner:
        case Permission.admin:
        case Permission.edit: {
            return (
                <div
                    className={`${
                        PERMISSION_CLASS_NAME[permissions.type]
                    } inline-block rounded-full h-6 px-1.75 py-0.5 ${className ?? ''}`}
                    onClick={onClick}
                    onMouseEnter={onMouseEnter}
                    onMouseLeave={onMouseLeave}
                >
                    {children}
                </div>
            )
        }
        case Permission.read:
        case Permission.view: {
            return (
                <div
                    className={`relative inline-block rounded-full ${className ?? ''}`}
                    onClick={onClick}
                    onMouseEnter={onMouseEnter}
                    onMouseLeave={onMouseLeave}
                >
                    {permissions.docs && (
                        <div className="border-permission-docs clip-path-top border-2 rounded-full absolute w-full h-full" />
                    )}
                    {permissions.execute && (
                        <div className="border-permission-exec clip-path-bottom border-2 rounded-full absolute w-full h-full" />
                    )}
                    <div
                        className={`${
                            PERMISSION_CLASS_NAME[permissions.type]
                        } rounded-full h-6 px-1.75 py-0.5 m-1`}
                    >
                        {children}
                    </div>
                </div>
            )
        }
    }
}
