/** @file A user and their permissions for a specific asset. */
import * as React from 'react'

import type * as text from '#/text'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

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

// ======================
// === UserPermission ===
// ======================

/** Props for a {@link UserPermission}. */
export interface UserPermissionProps {
  readonly asset: backendModule.AnySmartAsset
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
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [userPermission, setUserPermission] = React.useState(initialUserPermission)
  const assetTypeName = getText(ASSET_TYPE_TO_TEXT_ID[asset.type])

  React.useEffect(() => {
    setUserPermission(initialUserPermission)
  }, [initialUserPermission])

  const doSetUserPermission = async (newUserPermissions: backendModule.UserPermission) => {
    try {
      setUserPermission(newUserPermissions)
      outerSetUserPermission(newUserPermissions)
      await asset.setPermissions({
        actorsIds: [newUserPermissions.user.userId],
        action: newUserPermissions.permission,
      })
    } catch (error) {
      setUserPermission(userPermission)
      outerSetUserPermission(userPermission)
      toastAndLog('setPermissionsError', error, newUserPermissions.user.email)
    }
  }

  return (
    <div className="flex items-center gap-user-permission">
      <PermissionSelector
        showDelete
        disabled={isOnlyOwner && userPermission.user.userId === self.user.userId}
        error={isOnlyOwner ? getText('needsOwnerError', assetTypeName) : null}
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
      <span className="text">{userPermission.user.name}</span>
    </div>
  )
}
