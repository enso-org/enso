/** @file Naming utilities. */

// ==============
// === Naming ===
// ==============

/** Capitalizes first letter of the provided string. */
export function capitalizeFirstLetter(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1)
}

/** Converts the camel case name to kebab case one. For example, converts `myName` to `my-name`. */
export function camelToKebabCase(str: string) {
    return str.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()
}

/** Converts the camel case name to kebab case one. For example, converts `myName` to `My Name`. */
export function camelCaseToTitle(str: string) {
    return capitalizeFirstLetter(str.replace(/([a-z])([A-Z])/g, '$1 $2'))
}
