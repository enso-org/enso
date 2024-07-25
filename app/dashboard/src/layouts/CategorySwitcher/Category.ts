/** @file All posssible categories in the Data Catalog. */
import * as z from 'zod'

import { includesPredicate } from 'enso-common/src/utilities/data/array'

import LocalStorage from '#/utilities/LocalStorage'

// ================
// === Category ===
// ================

export const DRIVE_CATEGORIES = ['cloud', 'local', 'recent', 'trash'] as const

/** All posssible categories in the Data Catalog. */
export type DriveCategory = (typeof DRIVE_CATEGORIES)[number]

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
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
