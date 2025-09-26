/** @file Utilities for working with permissions. */
import type * as text from '../text.js'

// ========================
// === PermissionAction ===
// ========================

/** Backend representation of user permission types. */
export type PermissionAction =
  | 'Own'
  | 'Admin'
  | 'Edit'
  | 'Read'
  | 'Read_docs'
  | 'Read_exec'
  | 'View'
  | 'View_docs'
  | 'View_exec'

/** Whether each {@link PermissionAction} can execute a project. */
export const PERMISSION_ACTION_CAN_EXECUTE: Readonly<Record<PermissionAction, boolean>> = {
  ['Own']: true,
  ['Admin']: true,
  ['Edit']: true,
  ['Read']: false,
  ['Read_docs']: false,
  ['Read_exec']: true,
  ['View']: false,
  ['View_docs']: false,
  ['View_exec']: true,
}

// ==================
// === Permission ===
// ==================

/** Type of permission. This determines what kind of border is displayed. */
export type Permission = 'owner' | 'admin' | 'edit' | 'read' | 'view' | 'delete'

/** Precedences for each permission. A lower number means a higher priority. */
export const PERMISSION_PRECEDENCE: Readonly<Record<Permission, number>> = {
  ['owner']: 0,
  ['admin']: 1,
  ['edit']: 2,
  ['read']: 3,
  ['view']: 4,
  ['delete']: 1000,
}

/** Precedences for each permission action. A lower number means a higher priority. */
export const PERMISSION_ACTION_PRECEDENCE: Readonly<Record<PermissionAction, number>> = {
  ['Own']: 0,
  ['Admin']: 1,
  ['Edit']: 2,
  ['Read']: 3,
  ['Read_docs']: 4,
  ['Read_exec']: 5,
  ['View']: 6,
  ['View_docs']: 7,
  ['View_exec']: 8,
}

/** The corresponding {@link Permissions} for each {@link PermissionAction}. */
export const FROM_PERMISSION_ACTION: Readonly<Record<PermissionAction, Permissions>> = {
  ['Own']: { type: 'owner' },
  ['Admin']: { type: 'admin' },
  ['Edit']: { type: 'edit' },
  ['Read']: {
    type: 'read',
    execute: false,
    docs: false,
  },
  ['Read_docs']: {
    type: 'read',
    execute: false,
    docs: true,
  },
  ['Read_exec']: {
    type: 'read',
    execute: true,
    docs: false,
  },
  ['View']: {
    type: 'view',
    execute: false,
    docs: false,
  },
  ['View_docs']: {
    type: 'view',
    execute: false,
    docs: true,
  },
  ['View_exec']: {
    type: 'view',
    execute: true,
    docs: false,
  },
}

/**
 * The corresponding {@link PermissionAction} for each {@link Permission}.
 * Assumes no docs sub-permission and no execute sub-permission.
 */
export const TYPE_TO_PERMISSION_ACTION: Readonly<Record<Permission, PermissionAction>> = {
  ['owner']: 'Own',
  ['admin']: 'Admin',
  ['edit']: 'Edit',
  ['read']: 'Read',
  ['view']: 'View',
  // Should never happen, but provide a fallback just in case.
  ['delete']: 'View',
}

/**
 * The corresponding {@link text.TextId} for each {@link Permission}.
 * Assumes no docs sub-permission and no execute sub-permission.
 */
export const TYPE_TO_TEXT_ID: Readonly<Record<Permission, text.TextId>> = {
  ['owner']: 'ownerPermissionType',
  ['admin']: 'adminPermissionType',
  ['edit']: 'editPermissionType',
  ['read']: 'readPermissionType',
  ['view']: 'viewPermissionType',
  ['delete']: 'deletePermissionType',
} satisfies { [P in Permission]: `${P}PermissionType` }

/** The equivalent backend `PermissionAction` for a `Permissions`. */
export function toPermissionAction(permissions: Permissions): PermissionAction {
  switch (permissions.type) {
    case 'owner': {
      return 'Own'
    }
    case 'admin': {
      return 'Admin'
    }
    case 'edit': {
      return 'Edit'
    }
    case 'read': {
      return (
        permissions.execute ?
          permissions.docs ?
            /* should never happen, but use a fallback value */
            'Read_exec'
          : 'Read_exec'
        : permissions.docs ? 'Read_docs'
        : 'Read'
      )
    }
    case 'view': {
      return (
        permissions.execute ?
          permissions.docs ?
            /* should never happen, but use a fallback value */
            'View_exec'
          : 'View_exec'
        : permissions.docs ? 'View_docs'
        : 'View'
      )
    }
  }
}

// ===================
// === Permissions ===
// ===================

/** Properties common to all permissions. */
interface BasePermissions<T extends Permission> {
  readonly type: T
}

/** Owner permissions for an asset. */
type OwnerPermissions = BasePermissions<'owner'>

/** Admin permissions for an asset. */
type AdminPermissions = BasePermissions<'admin'>

/** Editor permissions for an asset. */
type EditPermissions = BasePermissions<'edit'>

/** Reader permissions for an asset. */
interface ReadPermissions extends BasePermissions<'read'> {
  readonly docs: boolean
  readonly execute: boolean
}

/** Viewer permissions for an asset. */
interface ViewPermissions extends BasePermissions<'view'> {
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
  type: 'view',
  docs: false,
  execute: false,
})
