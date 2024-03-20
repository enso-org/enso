/** @file A user and their permissions for a specific asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import PermissionSelector from '#/components/dashboard/PermissionSelector'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

/** Props for a {@link UserPermission}. */
export interface UserPermissionProps {
  readonly asset: backendModule.Asset
  readonly self: backendModule.UserPermission
  readonly isOnlyOwner: boolean
  readonly userPermission: backendModule.UserPermission
  readonly setUserPermission: (userPermissions: backendModule.UserPermission) => void
  readonly doDelete: (user: backendModule.UserInfo) => void
}

/** A user and their permissions for a specific asset. */
export default function UserPermission(props: UserPermissionProps) {
  const { asset, self, isOnlyOwner, doDelete } = props
  const { userPermission: initialUserPermission, setUserPermission: outerSetUserPermission } = props
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [userPermission, setUserPermission] = React.useState(initialUserPermission)

  React.useEffect(() => {
    setUserPermission(initialUserPermission)
  }, [initialUserPermission])

  const doSetUserPermission = async (newUserPermissions: backendModule.UserPermission) => {
    try {
      setUserPermission(newUserPermissions)
      outerSetUserPermission(newUserPermissions)
      await backend.createPermission({
        actorsIds: [newUserPermissions.user.sk],
        resourceId: asset.id,
        action: newUserPermissions.permission,
      })
    } catch (error) {
      setUserPermission(userPermission)
      outerSetUserPermission(userPermission)
      toastAndLog(`Could not set permissions of '${newUserPermissions.user.user_email}'`, error)
    }
  }

  return (
    <div className="flex items-center gap-user-permission">
      <PermissionSelector
        showDelete
        disabled={isOnlyOwner && userPermission.user.sk === self.user.sk}
        error={
          isOnlyOwner
            ? `This ${backendModule.ASSET_TYPE_NAME[asset.type]} must have at least one owner.`
            : null
        }
        selfPermission={self.permission}
        action={userPermission.permission}
        assetType={asset.type}
        onChange={async permissions => {
          await doSetUserPermission(object.merge(userPermission, { permission: permissions }))
        }}
        doDelete={() => {
          doDelete(userPermission.user)
        }}
      />
      <span className="text">{userPermission.user.user_name}</span>
    </div>
  )
}
