/** @file A horizontal selector for all possible permissions. */
import * as react from 'react'

import * as backend from '../backend'
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

// =============
// === Types ===
// =============

/** Props for a HTML `div` element. */
type DivProps = JSX.IntrinsicElements['div']

// ==========================
// === PermissionSelector ===
// ==========================

/** Props for a {@link PermissionSelector}. */
export interface PermissionSelectorProps extends Omit<DivProps, 'onChange'> {
    onChange: (permission: backend.PermissionAction) => void
    className?: string
    permissionClassName?: string
}

/** A horizontal selector for all possible permissions. */
function PermissionSelector(props: PermissionSelectorProps) {
    const { onChange, className, permissionClassName, ...passthroughProps } = props
    const [selectedPermission, setSelectedPermission] =
        react.useState<backend.PermissionAction | null>(null)

    return (
        <div className={`flex justify-items-center ${className ?? ''}`} {...passthroughProps}>
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
                                    selectedPermission === action ? 'shadow-soft' : ''
                                } ${permissionClassName ?? ''}`}
                            >
                                <div className="mx-1">{name}</div>
                            </PermissionDisplay>
                        </label>
                        <input
                            type="radio"
                            id={`share_with_permission_${action.toLowerCase()}`}
                            name="share_with_permission_input"
                            className="w-0 h-0"
                            onClick={() => {
                                setSelectedPermission(action)
                                onChange(action)
                            }}
                        />
                    </div>
                )
            })}
        </div>
    )
}

export default PermissionSelector
