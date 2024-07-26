/** @file Utilities for working with permissions. */
import { merge } from 'enso-common/src/utilities/data/object'
import * as permissions from 'enso-common/src/utilities/permissions'

import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import type Category from '#/layouts/CategorySwitcher/Category'

import * as backend from '#/services/Backend'

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

// ==========================
// === findSelfPermission ===
// ==========================

/** Try to find a permission belonging to the user. */
export function tryFindSelfPermission(
  self: backend.User,
  otherPermissions: readonly backend.AssetPermission[] | null
) {
  let selfPermission: backend.AssetPermission | null = null
  for (const permission of otherPermissions ?? []) {
    // `a >= b` means that `a` does not have more permissions than `b`.
    if (selfPermission && backend.compareAssetPermissions(selfPermission, permission) >= 0) {
      continue
    }
    if ('user' in permission && permission.user.userId !== self.userId) {
      continue
    }
    if (
      'userGroup' in permission &&
      (self.userGroups ?? []).every(groupId => groupId !== permission.userGroup.id)
    ) {
      continue
    }
    selfPermission = permission
  }
  return selfPermission
}

// ============================================
// === canPermissionModifyDirectoryContents ===
// ============================================

/** Whether the given permission means the user can edit the list of assets of the directory. */
export function canPermissionModifyDirectoryContents(permission: permissions.PermissionAction) {
  return (
    permission === permissions.PermissionAction.own ||
    permission === permissions.PermissionAction.admin ||
    permission === permissions.PermissionAction.edit
  )
}

// ==============================
// === replaceOwnerPermission ===
// ==============================

/** Replace the first owner permission with the permission of a new user or team. */
export function replaceOwnerPermission(
  asset: backend.AnyAsset,
  newOwner: backend.User | backend.UserGroupInfo
) {
  let found = false
  const newPermissions =
    asset.permissions?.map(permission => {
      if (found || permission.permission !== permissions.PermissionAction.own) {
        return permission
      } else {
        found = true
        if ('userId' in newOwner) {
          const newPermission: backend.UserPermission = {
            user: newOwner,
            permission: permissions.PermissionAction.own,
          }
          return newPermission
        } else {
          const newPermission: backend.UserGroupPermission = {
            userGroup: newOwner,
            permission: permissions.PermissionAction.own,
          }
          return newPermission
        }
      }
    }) ?? null
  return merge(asset, { permissions: newPermissions })
}

/** Find the new owner of an asset based on the path of its new parent directory. */
export function newOwnerFromPath(
  path: string,
  users: readonly backend.User[],
  userGroups: readonly backend.UserGroupInfo[]
) {
  const [, userName] = path.match(/enso:[/][/][/]Users[/]([^/]+)/) ?? []
  if (userName != null) {
    const userNameLowercase = userName.toLowerCase()
    return users.find(user => user.name.toLowerCase() === userNameLowercase)
  } else {
    const [, teamName] = path.match(/enso:[/][/][/]Teams[/]([^/]+)/) ?? []
    if (teamName != null) {
      const teamNameLowercase = teamName.toLowerCase()
      return userGroups.find(userGroup => userGroup.groupName === teamNameLowercase)
    } else {
      return
    }
  }
}
