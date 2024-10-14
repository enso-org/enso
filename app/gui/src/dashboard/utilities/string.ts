/** @file Utilities for manipulating strings. */

// =======================
// === capitalizeFirst ===
// =======================

/** Return the given string, but with the first letter uppercased. */
export function capitalizeFirst(string: string) {
  return string.replace(/^./, (match) => match.toUpperCase())
}

// ===================
// === regexEscape ===
// ===================

/** Sanitizes a string for use as a regex. */
export function regexEscape(string: string) {
  return string.replace(/[\\^$.|?*+()[{]/g, '\\$&')
}

// ========================
// === isWhitespaceOnly ===
// ========================

/** Whether a string consists only of whitespace, meaning that the string will not be visible. */
export function isWhitespaceOnly(string: string) {
  return /^\s*$/.test(string)
}

// ============================
// === camelCaseToTitleCase ===
// ============================

/**
 * Inserts spaces between every word, and capitalizes the first word.
 * DOES NOT make particles lowercase.
 */
export function camelCaseToTitleCase(string: string) {
  return string.replace(/([a-z0-9])([A-Z])/g, '$1 $2').replace(/^./, (c) => c.toUpperCase())
}

// ==============================
// === compareCaseInsensitive ===
// ==============================

/**
 * Return `1` if `a > b`, `-1` if `a < b`, and `0` if `a === b`.
 * Falls back to a case-sensitive comparison if the case-insensitive comparison returns `0`.
 */
export function compareCaseInsensitive(a: string, b: string) {
  const aLower = a.toLowerCase()
  const bLower = b.toLowerCase()
  return (
    aLower > bLower ? 1
    : aLower < bLower ? -1
    : a > b ? 1
    : a < b ? -1
    : 0
  )
}

// =====================
// === normalizeName ===
// =====================

/** Return a normalized name to check for duplicates. */
export function normalizeName(name: string) {
  return name.trim().replace(/\s+/g, ' ').toLowerCase()
}
