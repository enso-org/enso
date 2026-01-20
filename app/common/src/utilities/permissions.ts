/** @file Utilities for working with permissions. */
import {
  compareAssetPermissions,
  type AnyAsset,
  type AssetPermission,
  type User,
  type UserGroup,
} from '../services/Backend.js'
import type * as text from '../text.js'

/** Backend representation of user permission types. */
export enum PermissionAction {
  own = 'Own',
  admin = 'Admin',
  edit = 'Edit',
  read = 'Read',
  readAndDocs = 'Read_docs',
  readAndExec = 'Read_exec',
  view = 'View',
  viewAndDocs = 'View_docs',
  viewAndExec = 'View_exec',
}

/** Whether each {@link PermissionAction} can execute a project. */
export const PERMISSION_ACTION_CAN_EXECUTE: Readonly<Record<PermissionAction, boolean>> = {
  [PermissionAction.own]: true,
  [PermissionAction.admin]: true,
  [PermissionAction.edit]: true,
  [PermissionAction.read]: false,
  [PermissionAction.readAndDocs]: false,
  [PermissionAction.readAndExec]: true,
  [PermissionAction.view]: false,
  [PermissionAction.viewAndDocs]: false,
  [PermissionAction.viewAndExec]: true,
}

/** Type of permission. This determines what kind of border is displayed. */
export enum Permission {
  owner = 'owner',
  admin = 'admin',
  edit = 'edit',
  read = 'read',
  view = 'view',
  delete = 'delete',
}

/** Precedences for each permission. A lower number means a higher priority. */
export const PERMISSION_PRECEDENCE: Readonly<Record<Permission, number>> = {
  [Permission.owner]: 0,
  [Permission.admin]: 1,
  [Permission.edit]: 2,
  [Permission.read]: 3,
  [Permission.view]: 4,
  [Permission.delete]: 1000,
}

/** Precedences for each permission action. A lower number means a higher priority. */
export const PERMISSION_ACTION_PRECEDENCE: Readonly<Record<PermissionAction, number>> = {
  [PermissionAction.own]: 0,
  [PermissionAction.admin]: 1,
  [PermissionAction.edit]: 2,
  [PermissionAction.read]: 3,
  [PermissionAction.readAndDocs]: 4,
  [PermissionAction.readAndExec]: 5,
  [PermissionAction.view]: 6,
  [PermissionAction.viewAndDocs]: 7,
  [PermissionAction.viewAndExec]: 8,
}

/** The corresponding {@link Permissions} for each {@link PermissionAction}. */
export const FROM_PERMISSION_ACTION: Readonly<Record<PermissionAction, Permissions>> = {
  [PermissionAction.own]: { type: Permission.owner },
  [PermissionAction.admin]: { type: Permission.admin },
  [PermissionAction.edit]: { type: Permission.edit },
  [PermissionAction.read]: {
    type: Permission.read,
    execute: false,
    docs: false,
  },
  [PermissionAction.readAndDocs]: {
    type: Permission.read,
    execute: false,
    docs: true,
  },
  [PermissionAction.readAndExec]: {
    type: Permission.read,
    execute: true,
    docs: false,
  },
  [PermissionAction.view]: {
    type: Permission.view,
    execute: false,
    docs: false,
  },
  [PermissionAction.viewAndDocs]: {
    type: Permission.view,
    execute: false,
    docs: true,
  },
  [PermissionAction.viewAndExec]: {
    type: Permission.view,
    execute: true,
    docs: false,
  },
}

/**
 * The corresponding {@link PermissionAction} for each {@link Permission}.
 * Assumes no docs sub-permission and no execute sub-permission.
 */
export const TYPE_TO_PERMISSION_ACTION: Readonly<Record<Permission, PermissionAction>> = {
  [Permission.owner]: PermissionAction.own,
  [Permission.admin]: PermissionAction.admin,
  [Permission.edit]: PermissionAction.edit,
  [Permission.read]: PermissionAction.read,
  [Permission.view]: PermissionAction.view,
  // Should never happen, but provide a fallback just in case.
  [Permission.delete]: PermissionAction.view,
}

/**
 * The corresponding {@link text.TextId} for each {@link Permission}.
 * Assumes no docs sub-permission and no execute sub-permission.
 */
export const TYPE_TO_TEXT_ID: Readonly<Record<Permission, text.TextId>> = {
  [Permission.owner]: 'ownerPermissionType',
  [Permission.admin]: 'adminPermissionType',
  [Permission.edit]: 'editPermissionType',
  [Permission.read]: 'readPermissionType',
  [Permission.view]: 'viewPermissionType',
  [Permission.delete]: 'deletePermissionType',
} satisfies { [P in Permission]: `${P}PermissionType` }

/** The equivalent backend `PermissionAction` for a `Permissions`. */
export function toPermissionAction(permissions: Permissions): PermissionAction {
  switch (permissions.type) {
    case Permission.owner: {
      return PermissionAction.own
    }
    case Permission.admin: {
      return PermissionAction.admin
    }
    case Permission.edit: {
      return PermissionAction.edit
    }
    case Permission.read: {
      return (
        permissions.execute ?
          permissions.docs ?
            /* should never happen, but use a fallback value */
            PermissionAction.readAndExec
          : PermissionAction.readAndExec
        : permissions.docs ? PermissionAction.readAndDocs
        : PermissionAction.read
      )
    }
    case Permission.view: {
      return (
        permissions.execute ?
          permissions.docs ?
            /* should never happen, but use a fallback value */
            PermissionAction.viewAndExec
          : PermissionAction.viewAndExec
        : permissions.docs ? PermissionAction.viewAndDocs
        : PermissionAction.view
      )
    }
  }
}

/** Properties common to all permissions. */
interface BasePermissions<T extends Permission> {
  readonly type: T
}

/** Owner permissions for an asset. */
type OwnerPermissions = BasePermissions<Permission.owner>

/** Admin permissions for an asset. */
type AdminPermissions = BasePermissions<Permission.admin>

/** Editor permissions for an asset. */
type EditPermissions = BasePermissions<Permission.edit>

/** Reader permissions for an asset. */
interface ReadPermissions extends BasePermissions<Permission.read> {
  readonly docs: boolean
  readonly execute: boolean
}

/** Viewer permissions for an asset. */
interface ViewPermissions extends BasePermissions<Permission.view> {
  readonly docs: boolean
  readonly execute: boolean
}

/** Detailed permission information. This is used to draw the border. */
export type Permissions =
  | AdminPermissions
  | EditPermissions
  | OwnerPermissions
  | ReadPermissions
  | ViewPermissions

export const DEFAULT_PERMISSIONS: Permissions = Object.freeze({
  type: Permission.view,
  docs: false,
  execute: false,
})

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
export function tryGetOwnerPermission(asset: AnyAsset) {
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

/** Find the new owner of an asset based on the path of its new parent directory. */
export function newOwnerFromPath(
  path: string,
  users: readonly User[],
  userGroups: readonly UserGroup[],
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
