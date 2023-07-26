/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import * as backend from '../backend'
import * as permissionsModule from '../permissions'

// =================
// === Constants ===
// =================

/** CSS classes for each permission. */
export const PERMISSION_CLASS_NAME: Record<permissionsModule.Permission, string> = {
    [permissionsModule.Permission.owner]: 'text-tag-text bg-permission-owner',
    [permissionsModule.Permission.admin]: 'text-tag-text bg-permission-admin',
    [permissionsModule.Permission.edit]: 'text-tag-text bg-permission-edit',
    [permissionsModule.Permission.read]: 'text-tag-text bg-permission-read',
    [permissionsModule.Permission.view]: 'text-tag-text-2 bg-permission-view',
} as const

/** Precedences for each permission. A lower number means a higher priority. */
const PERMISSION_PRECEDENCE: Record<permissionsModule.Permission, number> = {
    // These are not magic numbers - they are just a sequence of numbers.
    /* eslint-disable @typescript-eslint/no-magic-numbers */
    [permissionsModule.Permission.owner]: 0,
    [permissionsModule.Permission.admin]: 1,
    [permissionsModule.Permission.edit]: 2,
    [permissionsModule.Permission.read]: 3,
    [permissionsModule.Permission.view]: 4,
    /* eslint-enable @typescript-eslint/no-magic-numbers */
}

/** The corresponding `Permissions` for each backend `PermissionAction`. */
export const PERMISSION: Record<backend.PermissionAction, permissionsModule.Permissions> = {
    [backend.PermissionAction.own]: { type: permissionsModule.Permission.owner },
    [backend.PermissionAction.execute]: {
        type: permissionsModule.Permission.read,
        execute: true,
        docs: false,
    },
    [backend.PermissionAction.edit]: { type: permissionsModule.Permission.edit },
    [backend.PermissionAction.view]: {
        type: permissionsModule.Permission.view,
        execute: false,
        docs: false,
    },
}

// ======================
// === permissionsToX ===
// ======================

/** Converts an array of {@link backend.PermissionAction} to a {@link permissionsModule.Permissions}. */
export function permissionActionsToPermissions(
    permissionActions: backend.PermissionAction[]
): permissionsModule.Permissions {
    return permissionActions.reduce<permissionsModule.Permissions>(
        (result, action) => {
            const actionResult = PERMISSION[action]
            return PERMISSION_PRECEDENCE[actionResult.type] <= PERMISSION_PRECEDENCE[result.type]
                ? actionResult
                : result
        },
        { type: permissionsModule.Permission.view, execute: false, docs: false }
    )
}

// =================
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps extends React.PropsWithChildren {
    permissions: permissionsModule.Permissions
    className?: string
    onClick?: React.MouseEventHandler<HTMLDivElement>
    onMouseEnter?: React.MouseEventHandler<HTMLDivElement>
    onMouseLeave?: React.MouseEventHandler<HTMLDivElement>
}

/** Colored border around icons and text indicating permissions. */
export default function PermissionDisplay(props: PermissionDisplayProps) {
    const { permissions, className, onClick, onMouseEnter, onMouseLeave, children } = props

    switch (permissions.type) {
        case permissionsModule.Permission.owner:
        case permissionsModule.Permission.admin:
        case permissionsModule.Permission.edit: {
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
        case permissionsModule.Permission.read:
        case permissionsModule.Permission.view: {
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
