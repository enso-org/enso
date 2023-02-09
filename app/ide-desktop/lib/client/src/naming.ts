// ==============
// === Naming ===
// ==============

export function capitalizeFirstLetter(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1)
}

export function camelToKebabCase(str: string) {
    return str.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()
}

export function camelCaseToTitle(str: string) {
    return capitalizeFirstLetter(str.replace(/([a-z])([A-Z])/g, '$1 $2'))
}
