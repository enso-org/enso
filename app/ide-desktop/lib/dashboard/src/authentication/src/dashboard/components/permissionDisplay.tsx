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
    regular = 'regular',
}

/** Base interface for all permissions. */
interface BasePermissions {
    type: Permission
}

/** Owner permissions over an asset. */
interface OwnerPermissions extends BasePermissions {
    type: Permission.owner
}

/** Admin permissions over an asset. */
interface AdminPermissions extends BasePermissions {
    type: Permission.admin
}

/** Regular permissions over an asset. */
interface RegularPermissions extends BasePermissions {
    type: Permission.regular
    read: boolean
    write: boolean
    docsWrite: boolean
    exec: boolean
}

/** Detailed permission information. This is used to draw the border. */
export type Permissions = AdminPermissions | OwnerPermissions | RegularPermissions

// =================
// === Constants ===
// =================

/** Classes common between all permission borders. */
const PERMISSION_BORDER_SHARED_CLASSES = 'border-2 rounded-full absolute w-full h-full'

/** The corresponding `Permissions` for each backend `PermissionAction`. */
export const PERMISSION: Record<backend.PermissionAction, Permissions> = {
    [backend.PermissionAction.own]: { type: Permission.owner },
    [backend.PermissionAction.execute]: {
        type: Permission.regular,
        read: false,
        write: false,
        docsWrite: false,
        exec: true,
    },
    [backend.PermissionAction.edit]: {
        type: Permission.regular,
        read: false,
        write: true,
        docsWrite: false,
        exec: false,
    },
    [backend.PermissionAction.view]: {
        type: Permission.regular,
        read: true,
        write: false,
        docsWrite: false,
        exec: false,
    },
}

// ======================
// === permissionsToX ===
// ======================

/** Converts an array of {@link backend.PermissionAction} to a {@link Permissions}. */
export function permissionActionsToPermissions(
    permissions: backend.PermissionAction[]
): Permissions {
    if (permissions.some(permission => permission === backend.PermissionAction.own)) {
        return { type: Permission.owner }
    } else {
        const result: Permissions = {
            type: Permission.regular,
            read: false,
            write: false,
            docsWrite: false,
            exec: false,
        }
        for (const permission of permissions) {
            switch (permission) {
                case backend.PermissionAction.own: {
                    // Ignored. This should never happen because of the `permissions.some` above.
                    break
                }
                case backend.PermissionAction.execute: {
                    result.exec = true
                    break
                }
                case backend.PermissionAction.edit: {
                    result.write = true
                    break
                }
                case backend.PermissionAction.view: {
                    result.read = true
                    break
                }
            }
        }
        return result
    }
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
function PermissionDisplay(props: PermissionDisplayProps) {
    const { permissions, className, onClick, onMouseEnter, onMouseLeave, children } = props
    let permissionBorder
    switch (permissions.type) {
        case Permission.owner: {
            permissionBorder = (
                <div className={`border-perm-owner ${PERMISSION_BORDER_SHARED_CLASSES}`}></div>
            )
            break
        }
        case Permission.admin: {
            permissionBorder = (
                <div className={`border-perm-admin ${PERMISSION_BORDER_SHARED_CLASSES}`}></div>
            )
            break
        }
        case Permission.regular: {
            permissionBorder = (
                <>
                    <div
                        className={`${
                            permissions.write ? 'border-perm-write' : 'border-perm-none'
                        } clip-path-top-left ${PERMISSION_BORDER_SHARED_CLASSES}`}
                    ></div>
                    <div
                        className={`${
                            permissions.read ? 'border-perm-read' : 'border-perm-none'
                        } clip-path-top-right ${PERMISSION_BORDER_SHARED_CLASSES}`}
                    ></div>
                    <div
                        className={`${
                            permissions.exec ? 'border-perm-exec' : 'border-perm-none'
                        } clip-path-bottom-left ${PERMISSION_BORDER_SHARED_CLASSES}`}
                    ></div>
                    <div
                        className={`${
                            permissions.docsWrite ? 'border-perm-docs-write' : 'border-perm-none'
                        } clip-path-bottom-right ${PERMISSION_BORDER_SHARED_CLASSES}`}
                    ></div>
                </>
            )
            break
        }
    }

    return (
        <div
            className={`mx-1 bg-white relative inline-block rounded-full ${className ?? ''}`}
            onClick={onClick}
            onMouseEnter={onMouseEnter}
            onMouseLeave={onMouseLeave}
        >
            {permissionBorder}
            <div className="bg-label rounded-full m-1">{children}</div>
        </div>
    )
}

export default PermissionDisplay
