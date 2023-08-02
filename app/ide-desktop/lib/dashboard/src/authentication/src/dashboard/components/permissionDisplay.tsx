/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import * as backend from '../backend'
import * as permissionsModule from '../permissions'

// ======================================
// === permissionActionsToPermissions ===
// ======================================

/** Converts an array of {@link backend.PermissionAction} to a {@link permissionsModule.Permissions}. */
export function permissionActionsToPermissions(
    permissionActions: backend.PermissionAction[]
): permissionsModule.Permissions {
    return permissionActions.reduce<permissionsModule.Permissions>(
        (result, action) => {
            const actionResult = permissionsModule.FROM_PERMISSION_ACTION[action]
            return permissionsModule.PERMISSION_PRECEDENCE[actionResult.type] <=
                permissionsModule.PERMISSION_PRECEDENCE[result.type]
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
                        permissionsModule.PERMISSION_CLASS_NAME[permissions.type]
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
                            permissionsModule.PERMISSION_CLASS_NAME[permissions.type]
                        } rounded-full h-6 px-1.75 py-0.5 m-1`}
                    >
                        {children}
                    </div>
                </div>
            )
        }
    }
}
