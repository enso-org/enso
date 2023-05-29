/** @file Colored border around icons and text indicating permissions. */
import * as react from 'react'

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
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps {
    permissions: Permissions
}

/** Colored border around icons and text indicating permissions. */
function PermissionDisplay(props: react.PropsWithChildren<PermissionDisplayProps>) {
    const { permissions, children } = props
    let permissionBorder
    switch (permissions.type) {
        case Permission.owner: {
            permissionBorder = (
                <div className="border-perm-owner border-2 rounded-full absolute w-full h-full"></div>
            )
            break
        }
        case Permission.admin: {
            permissionBorder = (
                <div className="border-perm-admin border-2 rounded-full absolute w-full h-full"></div>
            )
            break
        }
        case Permission.regular: {
            permissionBorder = (
                <>
                    <div
                        className={`${
                            permissions.write ? 'border-perm-write' : 'border-perm-none'
                        } clip-path-top-left border-2 rounded-full absolute w-full h-full`}
                    ></div>
                    <div
                        className={`${
                            permissions.read ? 'border-perm-read' : 'border-perm-none'
                        } clip-path-top-right border-2 rounded-full absolute w-full h-full`}
                    ></div>
                    <div
                        className={`${
                            permissions.exec ? 'border-perm-exec' : 'border-perm-none'
                        } clip-path-bottom-left border-2 rounded-full absolute w-full h-full`}
                    ></div>
                    <div
                        className={`${
                            permissions.docsWrite ? 'border-perm-docs-write' : 'border-perm-none'
                        } clip-path-bottom-right border-2 rounded-full absolute w-full h-full`}
                    ></div>
                </>
            )
            break
        }
    }

    return (
        <div className="m-1 relative inline-block">
            {permissionBorder}
            <div className="bg-label rounded-full m-1">{children}</div>
        </div>
    )
}

export default PermissionDisplay
