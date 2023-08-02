/** @file A user and their permissions for a specific asset. */
import * as React from 'react'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'

import PermissionSelector from './permissionSelector'

/** Props for a {@link UserPermissions}. */
export interface UserPermissionsProps {
    asset: backendModule.Asset
    userPermission: backendModule.UserPermission
    setUserPermission: (userPermissions: backendModule.UserPermission) => void
    doDelete: (user: backendModule.User) => void
}

/** A user and their permissions for a specific asset. */
export default function UserPermissions(props: UserPermissionsProps) {
    const {
        asset,
        userPermission: initialUserPermission,
        setUserPermission: outerSetUserPermission,
        doDelete,
    } = props
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [userPermissions, setUserPermissions] = React.useState(initialUserPermission)

    React.useEffect(() => {
        setUserPermissions(initialUserPermission)
    }, [initialUserPermission])

    const doSetUserPermission = async (newUserPermissions: backendModule.UserPermission) => {
        try {
            setUserPermissions(newUserPermissions)
            outerSetUserPermission(newUserPermissions)
            await backend.createPermission({
                userSubjects: [newUserPermissions.user.pk],
                resourceId: asset.id,
                action: newUserPermissions.permission,
            })
        } catch (error) {
            setUserPermissions(userPermissions)
            outerSetUserPermission(userPermissions)
            toastAndLog(
                `Unable to set permissions of '${newUserPermissions.user.user_email}'`,
                error
            )
        }
    }

    return (
        <div className="flex gap-3 items-center">
            <PermissionSelector
                allowDelete
                action={userPermissions.permission}
                assetType={asset.type}
                onChange={async permissions => {
                    await doSetUserPermission({
                        ...userPermissions,
                        permission: permissions,
                    })
                }}
                doDelete={() => {
                    doDelete(userPermissions.user)
                }}
            />
            <span className="leading-170 h-6 py-px">{userPermissions.user.user_name}</span>
        </div>
    )
}
