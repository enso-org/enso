/** @file A horizontal selector for all possible permissions. */
import * as react from 'react'

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
    initialPermissions?: Set<backend.PermissionAction> | null
    className?: string
    permissionClassName?: string
    onChange: (permissions: Set<backend.PermissionAction>) => void
}

/** A horizontal selector for all possible permissions. */
function PermissionSelector(props: PermissionSelectorProps) {
    const {
        initialPermissions: rawInitialPermissions,
        className,
        permissionClassName,
        onChange,
    } = props
    const [permissions, setPermissions] = react.useState(() => new Set<backend.PermissionAction>())

    react.useEffect(() => {
        if (rawInitialPermissions != null) {
            setPermissions(rawInitialPermissions)
            onChange(rawInitialPermissions)
        }
    }, [rawInitialPermissions])

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
                                const newPermissions = set.withPresence(
                                    permissions,
                                    action,
                                    element.checked
                                )
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
