/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import * as permissionsModule from '../permissions'

// =================
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps extends React.PropsWithChildren {
    action: permissionsModule.PermissionAction
    className?: string
    onClick?: React.MouseEventHandler<HTMLDivElement>
    onMouseEnter?: React.MouseEventHandler<HTMLDivElement>
    onMouseLeave?: React.MouseEventHandler<HTMLDivElement>
}

/** Colored border around icons and text indicating permissions. */
export default function PermissionDisplay(props: PermissionDisplayProps) {
    const { action, className, onClick, onMouseEnter, onMouseLeave, children } = props
    const permission = permissionsModule.FROM_PERMISSION_ACTION[action]

    switch (permission.type) {
        case permissionsModule.Permission.owner:
        case permissionsModule.Permission.admin:
        case permissionsModule.Permission.edit: {
            return (
                <div
                    className={`${
                        permissionsModule.PERMISSION_CLASS_NAME[permission.type]
                    } inline-block h-6 rounded-full px-1.75 py-0.5 ${className ?? ''}`}
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
                    {permission.docs && (
                        <div className="clip-path-top absolute h-full w-full rounded-full border-2 border-permission-docs" />
                    )}
                    {permission.execute && (
                        <div className="clip-path-bottom absolute h-full w-full rounded-full border-2 border-permission-exec" />
                    )}
                    <div
                        className={`${
                            permissionsModule.PERMISSION_CLASS_NAME[permission.type]
                        } m-1 h-6 rounded-full px-1.75 py-0.5`}
                    >
                        {children}
                    </div>
                </div>
            )
        }
    }
}
