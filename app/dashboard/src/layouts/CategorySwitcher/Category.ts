/** @file The categories available in the category switcher. */
import * as z from 'zod'

import { includesPredicate } from 'enso-common/src/utilities/data/array'

import LocalStorage from '#/utilities/LocalStorage'

// ================
// === Category ===
// ================

export const DRIVE_CATEGORIES = ['cloud', 'local', 'recent', 'trash'] as const

export type DriveCategory = (typeof DRIVE_CATEGORIES)[number]

export const isDriveCategory = includesPredicate(DRIVE_CATEGORIES)

// ============================
// === Globak configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly driveCategory: DriveCategory
  }
}

LocalStorage.registerKey('driveCategory', {
  schema: z.enum(DRIVE_CATEGORIES),
})

// ===============
// === isCloud ===
// ===============

/** Return `true` if the category is only accessible from the cloud.
 */
export function isCloudCategory(category: DriveCategory) {
  return category !== 'local'
}

/**
 * Return `true` if the category is only accessible locally.
 */
export function isLocalCategory(category: DriveCategory) {
  return category === 'local'
}
