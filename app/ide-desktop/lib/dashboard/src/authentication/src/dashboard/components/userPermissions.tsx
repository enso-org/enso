/** @file A user and their permissions for a specific asset. */
import * as React from 'react'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'

import PermissionSelector from './permissionSelector'

/** Props for a {@link UserPermissions}. */
export interface UserPermissionsProps {
    asset: backendModule.Asset
    userPermissions: backendModule.UserPermissions
    setUserPermissions: (userPermissions: backendModule.UserPermissions) => void
    doDelete: (user: backendModule.User) => void
}

/** A user and their permissions for a specific asset. */
export default function UserPermissions(props: UserPermissionsProps) {
    const {
        asset,
        userPermissions: initialUserPermissions,
        setUserPermissions: outerSetUserPermissions,
        doDelete,
    } = props
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [userPermissions, setUserPermissions] = React.useState(initialUserPermissions)

    React.useEffect(() => {
        setUserPermissions(initialUserPermissions)
    }, [initialUserPermissions])

    const doSetUserPermissions = async (newUserPermissions: backendModule.UserPermissions) => {
        try {
            setUserPermissions(newUserPermissions)
            outerSetUserPermissions(newUserPermissions)
            await backend.createPermission({
                userSubjects: [newUserPermissions.user.pk],
                resourceId: asset.id,
                actions: backendModule.permissionsToPermissionActions(
                    newUserPermissions.permissions
                ),
            })
        } catch (error) {
            setUserPermissions(userPermissions)
            outerSetUserPermissions(userPermissions)
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
                initialPermissions={userPermissions.permissions}
                assetType={asset.type}
                onChange={async permissions => {
                    await doSetUserPermissions({
                        ...userPermissions,
                        permissions,
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
