/** @file Utilities for working with permissions. */
import * as backend from '#/services/Backend'
import { directoryIdToUserGroupId, directoryIdToUserId } from '#/services/RemoteBackend'
import {
  type AssetPermission,
  compareAssetPermissions,
  type User,
} from 'enso-common/src/services/Backend'
import { Permission, PermissionAction } from 'enso-common/src/utilities/permissions'
import invariant from 'tiny-invariant'
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

/** Whether the given permission means the user can edit the list of assets of the directory. */
export function canPermissionModifyDirectoryContents(permission: PermissionAction) {
  return (
    permission === PermissionAction.own ||
    permission === PermissionAction.admin ||
    permission === PermissionAction.edit
  )
}

/** Replace the first owner permission with the permission of a new user or team. */
export function tryGetOwnerPermission(asset: backend.AnyAsset) {
  return asset.permissions?.find((permission) => permission.permission === PermissionAction.own)
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

/** Whether a path is inside a user's home directory. */
export function isUserParentsPath(path: backend.ParentsPath, userIds: readonly backend.UserId[]) {
  const rootFolder = path.split('/')[0]
  invariant(backend.isDirectoryId(rootFolder), 'Asset in user folder must have a root folder')
  const assetUserOrTeamId = directoryIdToUserId(rootFolder)
  return userIds.includes(assetUserOrTeamId)
}

/** Whether a path is inside a team's home directory. */
export function isTeamParentsPath(
  path: backend.ParentsPath,
  teamIds: readonly backend.UserGroupId[],
) {
  const rootFolder = path.split('/')[0]
  invariant(backend.isDirectoryId(rootFolder), 'Asset in team folder must have a root folder')
  const assetUserOrTeamId = directoryIdToUserGroupId(rootFolder)
  return teamIds.includes(assetUserOrTeamId)
}

/** Find the new owner of an asset based on the path of its new parent directory. */
export function newOwnerFromPath(
  path: string,
  users: readonly backend.User[],
  userGroups: readonly backend.UserGroup[],
) {
  const [, userName] = path.match(USER_PATH_REGEX) ?? []
  if (userName != null) {
    const userNameLowercase = userName.toLowerCase()
    return users.find((user) => user.name.toLowerCase() === userNameLowercase)
  } else {
    const [, teamName] = path.match(TEAM_PATH_REGEX) ?? []
    if (teamName != null) {
      const teamNameLowercase = teamName.toLowerCase()
      return userGroups.find((userGroup) => userGroup.name === teamNameLowercase)
    } else {
      return
    }
  }
}
