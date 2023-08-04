/** @file Utilities for manipulating strings. */

/** Return a function returning the singular or plural form of a word depending on the count of
 * items. */
export function makePluralize(singular: string, plural: string) {
    return (count: number) => (count === 1 ? singular : plural)
}

/** Return the given string, but with the first letter uppercased. */
export function capitalizeFirst(string: string) {
    return string.replace(/^./, match => match.toUpperCase())
}

/** Sanitizes a string for use as a regex. */
export function regexEscape(string: string) {
    return string.replace(/[\\^$.|?*+()[{]/g, '\\$&')
}
