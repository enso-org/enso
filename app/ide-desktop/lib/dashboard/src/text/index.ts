/** @file Functions related to displaying text. */

import ENGLISH from '#/text/english.json' assert { type: 'json' }

// =============
// === Types ===
// =============

/** Possible languages in which to display text. */
export enum Language {
  english = 'english',
}

/** An object containing the corresponding localized text for each text ID. */
type Texts = typeof ENGLISH
/** All possible text IDs. */
export type TextId = keyof Texts

/** Overrides the default number of placeholders (0). */
interface PlaceholderOverrides {
  copyAssetError: [string]
  moveAssetError: [string]
  findProjectError: [string]
  deleteAssetError: [string]
  restoreAssetError: [string]
  setPermissionsError: [string]
  unknownThreadIdError: [string]
  needsOwnerError: [string]
  inviteSuccess: [string]
  labelAssetDescription: [string]
  selectedAssetsDescription: [string, string]
  confirmDeletePrompt: [string]
  theAssetTypeTitle: [string, string]
}

/** An tuple of `string` for placeholders for each {@link TextId}. */
export interface Replacements
  extends PlaceholderOverrides,
    Record<Exclude<TextId, keyof PlaceholderOverrides>, []> {}

// =================
// === Constants ===
// =================

export const TEXTS: Readonly<Record<Language, Texts>> = {
  [Language.english]: ENGLISH,
}
