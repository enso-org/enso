/** @file CSS classes related to permissions. */
import { Permission } from 'enso-common/src/utilities/permissions'

/** CSS classes for each permission. */
export const PERMISSION_CLASS_NAME: Readonly<Record<Permission, string>> = {
  [Permission.owner]: 'text-tag-text bg-permission-owner',
  [Permission.admin]: 'text-tag-text bg-permission-admin',
  [Permission.edit]: 'text-tag-text bg-permission-edit',
  [Permission.read]: 'text-tag-text bg-permission-read',
  [Permission.view]: 'text-tag-text-2 bg-permission-view',
  [Permission.delete]: 'text-tag-text bg-delete',
}
