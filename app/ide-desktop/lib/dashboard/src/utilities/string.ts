/** @file Utilities for manipulating strings. */

// ========================
// === String utilities ===
// ========================

/** Return the given string, but with the first letter uppercased. */
export function capitalizeFirst(string: string) {
  return string.replace(/^./, match => match.toUpperCase())
}

/** Sanitizes a string for use as a regex. */
export function regexEscape(string: string) {
  return string.replace(/[\\^$.|?*+()[{]/g, '\\$&')
}

/** Whether a string consists only of whitespace, meaning that the string will not be visible. */
export function isWhitespaceOnly(string: string) {
  return /^\s*$/.test(string)
}

/** Whether a string consists only of printable ASCII. */
export function isPrintableASCIIOnly(string: string) {
  return /^[ -~]*$/.test(string)
}

/** Inserts spaces between every word, and capitalizes the first word.
 * DOES NOT make particles lowercase. */
export function camelCaseToTitleCase(string: string) {
  return string.replace(/([a-z0-9])([A-Z])/g, '$1 $2').replace(/^./, c => c.toUpperCase())
}
