/** @file Permissions for a specific user or user group on a specific asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import PermissionSelector from '#/components/dashboard/PermissionSelector'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

/** Props for a {@link Permissions}. */
export interface PermissionsProps {
  readonly asset: backendModule.Asset
  readonly self: backendModule.UserPermission
  readonly isOnlyOwner: boolean
  readonly permission: backendModule.AssetPermission
  readonly setPermission: (userPermissions: backendModule.AssetPermission) => void
  readonly doDelete: (user: backendModule.UserPermissionIdentifier) => void
}

/** Permissions for a specific user or user group on a specific asset. */
export default function Permissions(props: PermissionsProps) {
  const { asset, self, isOnlyOwner, doDelete } = props
  const { permission: initialUserPermission, setPermission: outerSetUserPermission } = props
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [permission, setPermission] = React.useState(initialUserPermission)
  const permissionId = backendModule.getAssetPermissionId(permission)

  React.useEffect(() => {
    setPermission(initialUserPermission)
  }, [initialUserPermission])

  const doSetPermission = async (newPermission: backendModule.AssetPermission) => {
    try {
      setPermission(newPermission)
      outerSetUserPermission(newPermission)
      await backend.createPermission({
        userSubjects: [backendModule.getAssetPermissionId(newPermission)],
        resourceId: asset.id,
        action: newPermission.permission,
      })
    } catch (error) {
      setPermission(newPermission)
      outerSetUserPermission(newPermission)
      const name = backendModule.getAssetPermissionName(newPermission)
      toastAndLog(`Could not set permissions of '${name}'`, error)
    }
  }

  return (
    <div className="flex gap-3 items-center">
      <PermissionSelector
        showDelete
        disabled={isOnlyOwner && permissionId === self.user.pk}
        error={
          isOnlyOwner
            ? `This ${backendModule.ASSET_TYPE_NAME[asset.type]} must have at least one owner.`
            : null
        }
        selfPermission={self.permission}
        action={permission.permission}
        assetType={asset.type}
        onChange={async permissions => {
          await doSetPermission(object.merge(permission, { permission: permissions }))
        }}
        doDelete={() => {
          doDelete(permissionId)
        }}
      />
      <span className="leading-170 h-6 py-px">
        {backendModule.getAssetPermissionName(permission)}
      </span>
    </div>
  )
}
