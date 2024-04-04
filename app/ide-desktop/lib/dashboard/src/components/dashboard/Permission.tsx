/** @file Permissions for a specific user or user group on a specific asset. */
import * as React from 'react'

import type * as text from '#/text'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import PermissionSelector from '#/components/dashboard/PermissionSelector'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

const ASSET_TYPE_TO_TEXT_ID: Readonly<Record<backendModule.AssetType, text.TextId>> = {
  [backendModule.AssetType.directory]: 'directoryAssetType',
  [backendModule.AssetType.project]: 'projectAssetType',
  [backendModule.AssetType.file]: 'fileAssetType',
  [backendModule.AssetType.secret]: 'secretAssetType',
  [backendModule.AssetType.dataLink]: 'connectorAssetType',
  [backendModule.AssetType.specialEmpty]: 'specialEmptyAssetType',
  [backendModule.AssetType.specialLoading]: 'specialLoadingAssetType',
} satisfies { [Type in backendModule.AssetType]: `${Type}AssetType` }

// ==================
// === Permission ===
// ==================

/** Props for a {@link Permission}. */
export interface PermissionProps {
  readonly asset: backendModule.Asset
  readonly self: backendModule.UserPermission
  readonly isOnlyOwner: boolean
  readonly permission: backendModule.AssetPermission
  readonly setPermission: (userPermissions: backendModule.AssetPermission) => void
  readonly doDelete: (user: backendModule.UserPermissionIdentifier) => void
}

/** A user or group, and their permissions for a specific asset. */
export default function Permission(props: PermissionProps) {
  const { asset, self, isOnlyOwner, doDelete } = props
  const { permission: initialPermission, setPermission: outerSetPermission } = props
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [permission, setPermission] = React.useState(initialPermission)
  const permissionId = backendModule.getAssetPermissionId(permission)
  const assetTypeName = getText(ASSET_TYPE_TO_TEXT_ID[asset.type])

  React.useEffect(() => {
    setPermission(initialPermission)
  }, [initialPermission])

  const doSetPermission = async (newPermission: backendModule.AssetPermission) => {
    try {
      setPermission(newPermission)
      outerSetPermission(newPermission)
      await backend.createPermission({
        actorsIds: [backendModule.getAssetPermissionId(newPermission)],
        resourceId: asset.id,
        action: newPermission.permission,
      })
    } catch (error) {
      setPermission(permission)
      outerSetPermission(permission)
      const name = backendModule.getAssetPermissionName(permission)
      toastAndLog('setPermissionsError', error, name)
    }
  }

  return (
    <div className="flex items-center gap-user-permission">
      <PermissionSelector
        showDelete
        disabled={isOnlyOwner && permissionId === self.user.userId}
        error={isOnlyOwner ? getText('needsOwnerError', assetTypeName) : null}
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
      <span className="text">{backendModule.getAssetPermissionName(permission)}</span>
    </div>
  )
}
