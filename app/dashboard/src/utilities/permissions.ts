/** @file Utilities for working with permissions. */
import {
  type AssetPermission,
  compareAssetPermissions,
  type User,
} from 'enso-common/src/services/Backend'
import * as permissions from 'enso-common/src/utilities/permissions'

export * from 'enso-common/src/utilities/permissions'

/** CSS classes for each permission. */
export const PERMISSION_CLASS_NAME: Readonly<Record<permissions.Permission, string>> = {
  [permissions.Permission.owner]: 'text-tag-text bg-permission-owner',
  [permissions.Permission.admin]: 'text-tag-text bg-permission-admin',
  [permissions.Permission.edit]: 'text-tag-text bg-permission-edit',
  [permissions.Permission.read]: 'text-tag-text bg-permission-read',
  [permissions.Permission.view]: 'text-tag-text-2 bg-permission-view',
  [permissions.Permission.delete]: 'text-tag-text bg-delete',
}

/** CSS classes for the docs permission. */
export const DOCS_CLASS_NAME = 'text-tag-text bg-permission-docs'
/** CSS classes for the execute permission. */
export const EXEC_CLASS_NAME = 'text-tag-text bg-permission-exec'

/** Try to find a permission belonging to the user. */
export function tryFindSelfPermission(
  self: User,
  otherPermissions: readonly AssetPermission[] | null,
) {
  let selfPermission: AssetPermission | null = null
  for (const permission of otherPermissions ?? []) {
    // `a >= b` means that `a` does not have more permissions than `b`.
    if (selfPermission && compareAssetPermissions(selfPermission, permission) >= 0) {
      continue
    }
    if ('user' in permission && permission.user.userId !== self.userId) {
      continue
    }
    if (
      'userGroup' in permission &&
      (self.userGroups ?? []).every((groupId) => groupId !== permission.userGroup.id)
    ) {
      continue
    }
    selfPermission = permission
  }
  return selfPermission
}
