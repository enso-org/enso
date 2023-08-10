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
export const PERMISSION_CLASS_NAME: Readonly<Record<Permission, string>> = {
    [Permission.owner]: 'text-tag-text bg-permission-owner',
    [Permission.admin]: 'text-tag-text bg-permission-admin',
    [Permission.edit]: 'text-tag-text bg-permission-edit',
    [Permission.read]: 'text-tag-text bg-permission-read',
    [Permission.view]: 'text-tag-text-2 bg-permission-view',
    [Permission.delete]: 'text-tag-text bg-delete',
} as const

/** Precedences for each permission. A lower number means a higher priority. */
export const PERMISSION_PRECEDENCE: Readonly<Record<Permission, number>> = {
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

/** Precedences for each permission action. A lower number means a higher priority. */
export const PERMISSION_ACTION_PRECEDENCE: Readonly<Record<backend.PermissionAction, number>> = {
    // These are not magic numbers - they are just a sequence of numbers.
    /* eslint-disable @typescript-eslint/no-magic-numbers */
    [backend.PermissionAction.own]: 0,
    [backend.PermissionAction.admin]: 1,
    [backend.PermissionAction.edit]: 2,
    [backend.PermissionAction.read]: 3,
    [backend.PermissionAction.readAndDocs]: 4,
    [backend.PermissionAction.readAndExec]: 5,
    [backend.PermissionAction.view]: 6,
    [backend.PermissionAction.viewAndDocs]: 7,
    [backend.PermissionAction.viewAndExec]: 8,
    /* eslint-enable @typescript-eslint/no-magic-numbers */
}

/** CSS classes for the docs permission. */
export const DOCS_CLASS_NAME = 'text-tag-text bg-permission-docs'
/** CSS classes for the execute permission. */
export const EXEC_CLASS_NAME = 'text-tag-text bg-permission-exec'

/** The corresponding {@link Permissions} for each {@link backend.PermissionAction}. */
export const FROM_PERMISSION_ACTION: Readonly<
    Record<backend.PermissionAction, Readonly<Permissions>>
> = {
    [backend.PermissionAction.own]: { type: Permission.owner },
    [backend.PermissionAction.admin]: { type: Permission.admin },
    [backend.PermissionAction.edit]: { type: Permission.edit },
    [backend.PermissionAction.read]: {
        type: Permission.read,
        execute: false,
        docs: false,
    },
    [backend.PermissionAction.readAndDocs]: {
        type: Permission.read,
        execute: false,
        docs: true,
    },
    [backend.PermissionAction.readAndExec]: {
        type: Permission.read,
        execute: true,
        docs: false,
    },
    [backend.PermissionAction.view]: {
        type: Permission.view,
        execute: false,
        docs: false,
    },
    [backend.PermissionAction.viewAndDocs]: {
        type: Permission.view,
        execute: false,
        docs: true,
    },
    [backend.PermissionAction.viewAndExec]: {
        type: Permission.view,
        execute: true,
        docs: false,
    },
}

/** The corresponding {@link backend.PermissionAction} for each {@link Permission}.
 * Assumes no docs sub-permission and no execute sub-permission. */
export const TYPE_TO_PERMISSION_ACTION: Readonly<Record<Permission, backend.PermissionAction>> = {
    [Permission.owner]: backend.PermissionAction.own,
    [Permission.admin]: backend.PermissionAction.admin,
    [Permission.edit]: backend.PermissionAction.edit,
    [Permission.read]: backend.PermissionAction.read,
    [Permission.view]: backend.PermissionAction.view,
    // SHould never happen, but provide a fallback just in case.
    [Permission.delete]: backend.PermissionAction.view,
}

/** The equivalent backend `PermissionAction` for a `Permissions`. */
export function toPermissionAction(permissions: Permissions): backend.PermissionAction {
    switch (permissions.type) {
        case Permission.owner: {
            return backend.PermissionAction.own
        }
        case Permission.admin: {
            return backend.PermissionAction.admin
        }
        case Permission.edit: {
            return backend.PermissionAction.edit
        }
        case Permission.read: {
            return permissions.execute
                ? permissions.docs
                    ? /* should never happen, but use a fallback value */
                      backend.PermissionAction.readAndExec
                    : backend.PermissionAction.readAndExec
                : permissions.docs
                ? backend.PermissionAction.readAndDocs
                : backend.PermissionAction.read
        }
        case Permission.view: {
            return permissions.execute
                ? permissions.docs
                    ? /* should never happen, but use a fallback value */
                      backend.PermissionAction.viewAndExec
                    : backend.PermissionAction.viewAndExec
                : permissions.docs
                ? backend.PermissionAction.viewAndDocs
                : backend.PermissionAction.view
        }
    }
}

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

export const DEFAULT_PERMISSIONS: Readonly<Permissions> = {
    type: Permission.view,
    docs: false,
    execute: false,
}

// ======================================
// === tryGetSingletonOwnerPermission ===
// ======================================

/** Return an array containing the owner permission if `owner` is not `null`,
 * else return an empty array (`[]`). */
export function tryGetSingletonOwnerPermission(
    owner: backend.UserOrOrganization | null,
    user: backend.SimpleUser | null
): backend.UserPermission[] {
    return owner != null
        ? [
              {
                  user: {
                      // The names are defined by the backend and cannot be changed.
                      /* eslint-disable @typescript-eslint/naming-convention */
                      pk: user?.id ?? backend.Subject(''),
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
