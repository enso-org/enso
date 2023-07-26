/** @file A selector for all possible permissions. */
import * as React from 'react'

import * as permissionsModule from '../permissions'
import PermissionTypeSelector from './permissionTypeSelector'

// ==========================
// === PermissionSelector ===
// ==========================

/** Props for a {@link PermissionSelector}. */
export interface PermissionSelectorProps {
    /** If this prop changes, the internal state will be updated too. */
    initialPermissions?: permissionsModule.Permissions | null
    className?: string
    onChange: (permissions: permissionsModule.Permissions) => void
}

/** A horizontal selector for all possible permissions. */
export default function PermissionSelector(props: PermissionSelectorProps) {
    const { initialPermissions, className, onChange } = props
    const [permissions, setPermissions] = React.useState<permissionsModule.Permissions>(
        initialPermissions ?? permissionsModule.DEFAULT_PERMISSIONS
    )
    const [TheChild, setTheChild] = React.useState<(() => JSX.Element) | null>()

    React.useEffect(() => {
        onChange(permissions)
    }, [onChange, permissions])

    const showPermissionTypeSelector = React.useCallback(() => {
        setTheChild(
            () =>
                function Child() {
                    return (
                        <PermissionTypeSelector
                            type={permissions.type}
                            onChange={type => {
                                setTheChild(null)
                                let newPermissions: permissionsModule.Permissions
                                switch (type) {
                                    case permissionsModule.Permission.read:
                                    case permissionsModule.Permission.view: {
                                        newPermissions = { type, docs: false, execute: false }
                                        break
                                    }
                                    default: {
                                        newPermissions = { type }
                                        break
                                    }
                                }
                                setPermissions(newPermissions)
                            }}
                        />
                    )
                }
        )
    }, [permissions.type])

    let permissionDisplay: JSX.Element

    switch (permissions.type) {
        case permissionsModule.Permission.read:
        case permissionsModule.Permission.view: {
            permissionDisplay = (
                <div className="flex gap-px w-30.25">
                    <button
                        className={`grow rounded-l-full h-6 px-1.75 py-0.5 ${
                            permissionsModule.PERMISSION_CLASS_NAME[permissions.type]
                        }`}
                        onClick={showPermissionTypeSelector}
                    >
                        {permissions.type}
                    </button>
                    <button
                        className={`grow h-6 px-1.75 py-0.5 ${permissionsModule.DOCS_CLASS_NAME} ${
                            permissions.docs ? '' : 'opacity-30'
                        }`}
                        onClick={event => {
                            event.stopPropagation()
                            setPermissions({ ...permissions, docs: !permissions.docs })
                        }}
                    >
                        docs
                    </button>
                    <button
                        className={`grow rounded-r-full h-6 px-1.75 py-0.5 ${
                            permissionsModule.EXEC_CLASS_NAME
                        } ${permissions.execute ? '' : 'opacity-30'}`}
                        onClick={event => {
                            event.stopPropagation()
                            setPermissions({ ...permissions, execute: !permissions.execute })
                        }}
                    >
                        exec
                    </button>
                </div>
            )
            break
        }
        default: {
            permissionDisplay = (
                <button
                    className={`${
                        permissionsModule.PERMISSION_CLASS_NAME[permissions.type]
                    } rounded-full w-30.25`}
                    onClick={showPermissionTypeSelector}
                >
                    {permissions.type}
                </button>
            )
            break
        }
    }

    return (
        <div className={className}>
            {permissionDisplay}
            {TheChild && <TheChild />}
        </div>
    )
}
