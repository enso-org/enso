/** @file Naming utilities. */

// ==============
// === Naming ===
// ==============

/** Capitalize the first letter of the provided string. */
export function capitalizeFirstLetter(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1)
}

/** Convert a camel case name to kebab case. For example, converts `myName` to `my-name`. */
export function camelToKebabCase(str: string) {
    return str.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()
}

/** Convert a camel case name to title case. For example, converts `myName` to `My Name`. */
export function camelCaseToTitle(str: string) {
    return capitalizeFirstLetter(str.replace(/([a-z])([A-Z])/g, '$1 $2'))
}
