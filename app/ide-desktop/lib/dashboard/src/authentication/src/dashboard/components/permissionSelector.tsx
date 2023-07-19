/** @file A horizontal selector for all possible permissions. */
import * as React from 'react'

import * as backend from '../backend'
import * as set from '../../set'

import PermissionDisplay, * as permissionDisplay from './permissionDisplay'

// =================
// === Constants ===
// =================

/** A list of all permissions, and relevant associated data. */
const PERMISSIONS = [
    // `name` currently holds the same as `action`. However, it may need to change in the future,
    // for features like internalization, so it is kept separate.
    { action: backend.PermissionAction.own, name: 'Own' },
    { action: backend.PermissionAction.execute, name: 'Execute' },
    { action: backend.PermissionAction.edit, name: 'Edit' },
    { action: backend.PermissionAction.view, name: 'View' },
].map(object => ({
    ...object,
    permission: permissionDisplay.PERMISSION[object.action],
}))

// ==========================
// === PermissionSelector ===
// ==========================

/** Props for a {@link PermissionSelector}. */
export interface PermissionSelectorProps {
    /** If this prop changes, the internal state will be updated too. */
    initialPermissions?: backend.PermissionAction[] | null
    className?: string
    permissionClassName?: string
    onChange: (permissions: Set<backend.PermissionAction>) => void
}

/** A horizontal selector for all possible permissions. */
function PermissionSelector(props: PermissionSelectorProps) {
    const { initialPermissions, className, permissionClassName, onChange } = props
    const [permissions, setPermissions] = React.useState(() => new Set<backend.PermissionAction>())

    React.useEffect(() => {
        if (initialPermissions != null) {
            const initialPermissionsSet = new Set(initialPermissions)
            setPermissions(initialPermissionsSet)
            onChange(initialPermissionsSet)
        }
        // `onChange` is NOT a dependency.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [initialPermissions])

    return (
        <div className={`flex justify-items-center ${className ?? ''}`}>
            {PERMISSIONS.map(object => {
                const { name, action, permission } = object

                return (
                    <div className="flex flex-1" key={action}>
                        <label
                            htmlFor={`share_with_permission_${action.toLowerCase()}`}
                            className="m-auto"
                        >
                            <PermissionDisplay
                                permissions={permission}
                                className={`cursor-pointer ${
                                    permissions.has(action) ? 'shadow-soft-dark' : ''
                                } ${permissionClassName ?? ''}`}
                            >
                                <div className="mx-1">{name}</div>
                            </PermissionDisplay>
                        </label>
                        <input
                            type="checkbox"
                            checked={permissions.has(action)}
                            id={`share_with_permission_${action.toLowerCase()}`}
                            name="share_with_permission_input"
                            className="w-0 h-0"
                            onChange={event => {
                                const element = event.currentTarget
                                let newPermissions: Set<backend.PermissionAction>
                                if (action === backend.PermissionAction.own) {
                                    newPermissions = new Set(element.checked ? [action] : [])
                                } else {
                                    newPermissions = set.withPresence(
                                        permissions,
                                        action,
                                        element.checked
                                    )
                                    newPermissions.delete(backend.PermissionAction.own)
                                }
                                setPermissions(newPermissions)
                                onChange(newPermissions)
                            }}
                        />
                    </div>
                )
            })}
        </div>
    )
}

export default PermissionSelector
