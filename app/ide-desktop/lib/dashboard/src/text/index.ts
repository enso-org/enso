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

// =================
// === Constants ===
// =================

export const TEXTS: Readonly<Record<Language, Texts>> = {
    [Language.english]: ENGLISH,
}
