/** @file Utilities for working with permissions. */
import * as permissions from 'enso-common/src/utilities/permissions'

import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import type Category from '#/layouts/CategorySwitcher/Category'

import type * as backend from '#/services/Backend'

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

// ======================================
// === tryGetSingletonOwnerPermission ===
// ======================================

/** Return an array containing the owner permission if `owner` is not `null`,
 * else return an empty array (`[]`). */
export function tryGetSingletonOwnerPermission(
  owner: backend.User | null,
  category: Category
): readonly backend.AssetPermission[] {
  switch (category.type) {
    case categoryModule.CategoryType.team: {
      return [{ userGroup: category.team, permission: permissions.PermissionAction.own }]
    }
    default: {
      if (owner != null) {
        const { organizationId, userId, name, email } = owner
        return [
          {
            user: { organizationId, userId, name, email },
            permission: permissions.PermissionAction.own,
          },
        ]
      } else {
        return []
      }
    }
  }
}
