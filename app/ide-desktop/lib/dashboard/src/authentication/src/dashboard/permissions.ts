/** @file Utilities for working with permissions. */
import * as backend from './backend'

// ==================
// === Permission ===
// ==================

/** Type of permission. This determines what kind of border is displayed. */
export enum Permission {
    owner = 'owner',
    admin = 'admin',
    edit = 'edit',
    read = 'read',
    view = 'view',
}

/** CSS classes for each permission. */
export const PERMISSION_CLASS_NAME: Record<Permission, string> = {
    [Permission.owner]: 'text-tag-text bg-permission-owner',
    [Permission.admin]: 'text-tag-text bg-permission-admin',
    [Permission.edit]: 'text-tag-text bg-permission-edit',
    [Permission.read]: 'text-tag-text bg-permission-read',
    [Permission.view]: 'text-tag-text-2 bg-permission-view',
} as const

// ===================
// === Permissions ===
// ===================

/** Properties common to all permissions. */
interface BasePermissions<T extends Permission> {
    type: T
}

/** Owner permissions for an asset. */
interface OwnerPermissions extends BasePermissions<Permission.owner> {}

/** Admin permissions for an asset. */
interface AdminPermissions extends BasePermissions<Permission.admin> {}

/** Editor permissions for an asset. */
interface EditPermissions extends BasePermissions<Permission.edit> {}

/** Reader permissions for an asset. */
interface ReadPermissions extends BasePermissions<Permission.read> {
    execute: boolean
    docs: boolean
}

/** Viewer permissions for an asset. */
interface ViewPermissions extends BasePermissions<Permission.view> {
    execute: boolean
    docs: boolean
}

/** Detailed permission information. This is used to draw the border. */
export type Permissions =
    | AdminPermissions
    | EditPermissions
    | OwnerPermissions
    | ReadPermissions
    | ViewPermissions

// ======================================
// === tryGetSingletonOwnerPermission ===
// ======================================

/** Returns an array containing the owner permission if `owner` is not `null`;
 * else returns an empty array (`[]`). */
export function tryGetSingletonOwnerPermission(owner: backend.UserOrOrganization | null) {
    return owner != null
        ? [
              {
                  user: {
                      // The names are defined by the backend and cannot be changed.
                      /* eslint-disable @typescript-eslint/naming-convention */
                      pk: backend.Subject(''),
                      organization_id: owner.id,
                      user_email: owner.email,
                      user_name: owner.name,
                      /* eslint-enable @typescript-eslint/naming-convention */
                  },
                  permission: backend.PermissionAction.own,
              },
          ]
        : []
}
