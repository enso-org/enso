/** @file Permissions for a specific user or user group on a specific asset. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import type * as text from 'enso-common/src/text'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

import PermissionSelector from '#/components/dashboard/PermissionSelector'
import FocusArea from '#/components/styled/FocusArea'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import { Text } from '#/components/AriaComponents'
import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

const ASSET_TYPE_TO_TEXT_ID: Readonly<Record<backendModule.AssetType, text.TextId>> = {
  [backendModule.AssetType.directory]: 'directoryAssetType',
  [backendModule.AssetType.project]: 'projectAssetType',
  [backendModule.AssetType.file]: 'fileAssetType',
  [backendModule.AssetType.secret]: 'secretAssetType',
  [backendModule.AssetType.specialEmpty]: 'specialEmptyAssetType',
  [backendModule.AssetType.specialError]: 'specialErrorAssetType',
  [backendModule.AssetType.specialLoading]: 'specialLoadingAssetType',
  [backendModule.AssetType.datalink]: 'datalinkAssetType',
} satisfies { [Type in backendModule.AssetType]: `${Type}AssetType` }

// ==================
// === Permission ===
// ==================

/** Props for a {@link Permission}. */
export interface PermissionProps {
  readonly backend: Backend
  readonly asset: Pick<backendModule.Asset, 'id' | 'permissions' | 'type'>

  readonly self: backendModule.AssetPermission
  readonly isOnlyOwner: boolean
  readonly permission: backendModule.AssetPermission
  readonly setPermission: (userPermissions: backendModule.AssetPermission) => void
  readonly doDelete: (user: backendModule.UserPermissionIdentifier) => void
}

/** A user or group, and their permissions for a specific asset. */
export default function Permission(props: PermissionProps) {
  const { backend, asset, self, isOnlyOwner, doDelete } = props
  const { permission: initialPermission, setPermission: outerSetPermission } = props
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [permission, setPermission] = React.useState(initialPermission)
  const permissionId = backendModule.getAssetPermissionId(permission)
  const isDisabled = isOnlyOwner && backendModule.getAssetPermissionId(self) === permissionId
  const assetTypeName = getText(ASSET_TYPE_TO_TEXT_ID[asset.type])

  const createPermission = useMutation(
    backendMutationOptions(backend, 'createPermission'),
  ).mutateAsync

  React.useEffect(() => {
    setPermission(initialPermission)
  }, [initialPermission])

  const doSetPermission = async (newPermission: backendModule.AssetPermission) => {
    try {
      setPermission(newPermission)
      outerSetPermission(newPermission)
      await createPermission([
        {
          actorsIds: [backendModule.getAssetPermissionId(newPermission)],
          resourceId: asset.id,
          action: newPermission.permission,
        },
      ])
    } catch (error) {
      setPermission(permission)
      outerSetPermission(permission)
      toastAndLog('setPermissionsError', error)
    }
  }

  return (
    <FocusArea active={!isDisabled} direction="horizontal">
      {(innerProps) => (
        <div className="flex w-full items-center gap-user-permission" {...innerProps}>
          <PermissionSelector
            showDelete
            isDisabled={isDisabled}
            error={isOnlyOwner ? getText('needsOwnerError', assetTypeName) : null}
            selfPermission={self.permission}
            action={permission.permission}
            assetType={asset.type}
            onChange={async (permissions) => {
              await doSetPermission(object.merge(permission, { permission: permissions }))
            }}
            doDelete={() => {
              doDelete(backendModule.getAssetPermissionId(permission))
            }}
          />
          <Text truncate="1">{backendModule.getAssetPermissionName(permission)}</Text>
        </div>
      )}
    </FocusArea>
  )
}
