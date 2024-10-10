/** @file Utilities for working with permissions. */
import type { Category } from '#/layouts/CategorySwitcher/Category'
import * as backend from '#/services/Backend'
import {
  type AssetPermission,
  compareAssetPermissions,
  type User,
} from 'enso-common/src/services/Backend'
import { merge } from 'enso-common/src/utilities/data/object'
import { Permission, PermissionAction } from 'enso-common/src/utilities/permissions'
export * from 'enso-common/src/utilities/permissions'

/** CSS classes for each permission. */
export const PERMISSION_CLASS_NAME: Readonly<Record<Permission, string>> = {
  [Permission.owner]: 'text-tag-text bg-permission-owner',
  [Permission.admin]: 'text-tag-text bg-permission-admin',
  [Permission.edit]: 'text-tag-text bg-permission-edit',
  [Permission.read]: 'text-tag-text bg-permission-read',
  [Permission.view]: 'text-tag-text-2 bg-permission-view',
  [Permission.delete]: 'text-tag-text bg-delete',
}

/** CSS classes for the docs permission. */
export const DOCS_CLASS_NAME = 'text-tag-text bg-permission-docs'
/** CSS classes for the execute permission. */
export const EXEC_CLASS_NAME = 'text-tag-text bg-permission-exec'

// ================================
// === tryCreateOwnerPermission ===
// ================================

/**
 * Return an array containing the owner permission if `owner` is not `null`,
 * else return an empty array (`[]`).
 */
export function tryCreateOwnerPermission(
  path: string,
  category: Category,
  user: backend.User,
  users: readonly backend.User[],
  userGroups: readonly backend.UserGroupInfo[],
): readonly backend.AssetPermission[] {
  switch (category.type) {
    case 'team': {
      return [{ userGroup: category.team, permission: PermissionAction.own }]
    }
    default: {
      const isFreeOrSolo =
        user.plan == null || user.plan === backend.Plan.free || user.plan === backend.Plan.solo
      const owner = isFreeOrSolo ? user : newOwnerFromPath(path, users, userGroups) ?? user
      if ('userId' in owner) {
        const { organizationId, userId, name, email } = owner
        return [{ user: { organizationId, userId, name, email }, permission: PermissionAction.own }]
      } else {
        return [{ userGroup: owner, permission: PermissionAction.own }]
      }
    }
  }
}

// ==========================
// === findSelfPermission ===
// ==========================

/** Try to find a permission belonging to the user. */
export function tryFindSelfPermission(
  self: User,
  otherPermissions: readonly AssetPermission[] | null | undefined,
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

// ============================================
// === canPermissionModifyDirectoryContents ===
// ============================================

/** Whether the given permission means the user can edit the list of assets of the directory. */
export function canPermissionModifyDirectoryContents(permission: PermissionAction) {
  return (
    permission === PermissionAction.own ||
    permission === PermissionAction.admin ||
    permission === PermissionAction.edit
  )
}

// ==============================
// === replaceOwnerPermission ===
// ==============================

/** Replace the first owner permission with the permission of a new user or team. */
export function replaceOwnerPermission(
  asset: backend.AnyAsset,
  newOwner: backend.User | backend.UserGroupInfo,
) {
  let found = false
  const newPermissions =
    asset.permissions?.map((permission) => {
      if (found || permission.permission !== PermissionAction.own) {
        return permission
      } else {
        found = true
        if ('userId' in newOwner) {
          const newPermission: backend.UserPermission = {
            user: newOwner,
            permission: PermissionAction.own,
          }
          return newPermission
        } else {
          const newPermission: backend.UserGroupPermission = {
            userGroup: newOwner,
            permission: PermissionAction.own,
          }
          return newPermission
        }
      }
    }) ?? null
  return merge(asset, { permissions: newPermissions })
}

const USER_PATH_REGEX = /^enso:[/][/][/]Users[/]([^/]+)/
const TEAM_PATH_REGEX = /^enso:[/][/][/]Teams[/]([^/]+)/

/** Whether a path is inside a user's home directory. */
export function isUserPath(path: string) {
  return USER_PATH_REGEX.test(path)
}

/** Whether a path is inside a team's home directory. */
export function isTeamPath(path: string) {
  return TEAM_PATH_REGEX.test(path)
}

/** Find the new owner of an asset based on the path of its new parent directory. */
export function newOwnerFromPath(
  path: string,
  users: readonly backend.User[],
  userGroups: readonly backend.UserGroupInfo[],
) {
  const [, userName] = path.match(USER_PATH_REGEX) ?? []
  if (userName != null) {
    const userNameLowercase = userName.toLowerCase()
    return users.find((user) => user.name.toLowerCase() === userNameLowercase)
  } else {
    const [, teamName] = path.match(TEAM_PATH_REGEX) ?? []
    if (teamName != null) {
      const teamNameLowercase = teamName.toLowerCase()
      return userGroups.find((userGroup) => userGroup.groupName === teamNameLowercase)
    } else {
      return
    }
  }
}
