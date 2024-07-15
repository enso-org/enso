/** @file Utilities for working with permissions. */

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
