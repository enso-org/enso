/** @file Utilities for working with permissions. */
import * as backend from './backend'

/** This file MUST be `.tsx` even though it does not contain JSX, in order for Tailwind to include
 * the classes in this file. */

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
    delete = 'delete',
}

/** CSS classes for each permission. */
export const PERMISSION_CLASS_NAME: Record<Permission, string> = {
    [Permission.owner]: 'text-tag-text bg-permission-owner',
    [Permission.admin]: 'text-tag-text bg-permission-admin',
    [Permission.edit]: 'text-tag-text bg-permission-edit',
    [Permission.read]: 'text-tag-text bg-permission-read',
    [Permission.view]: 'text-tag-text-2 bg-permission-view',
    [Permission.delete]: 'text-tag-text bg-delete',
} as const

/** Precedences for each permission. A lower number means a higher priority. */
export const PERMISSION_PRECEDENCE: Record<Permission, number> = {
    // These are not magic numbers - they are just a sequence of numbers.
    /* eslint-disable @typescript-eslint/no-magic-numbers */
    [Permission.owner]: 0,
    [Permission.admin]: 1,
    [Permission.edit]: 2,
    [Permission.read]: 3,
    [Permission.view]: 4,
    [Permission.delete]: 1000,
    /* eslint-enable @typescript-eslint/no-magic-numbers */
}

/** CSS classes for the docs permission. */
export const DOCS_CLASS_NAME = 'text-tag-text bg-permission-docs'
/** CSS classes for the execute permission. */
export const EXEC_CLASS_NAME = 'text-tag-text bg-permission-exec'

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
    docs: boolean
    execute: boolean
}

/** Viewer permissions for an asset. */
interface ViewPermissions extends BasePermissions<Permission.view> {
    docs: boolean
    execute: boolean
}

/** Detailed permission information. This is used to draw the border. */
export type Permissions =
    | AdminPermissions
    | EditPermissions
    | OwnerPermissions
    | ReadPermissions
    | ViewPermissions

export const DEFAULT_PERMISSIONS = Object.freeze<Permissions>({
    type: Permission.view,
    docs: false,
    execute: false,
})

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
