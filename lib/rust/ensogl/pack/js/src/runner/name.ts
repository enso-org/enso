/** @file Naming utilities. Defines name mangling and un-mangling functions allowing passing complex
 * UTF-8 names between Rust and JS. */

export const NAME_REGEX = new RegExp(String.raw`(?<underscore>__)|(_(?<specialChar>[0-9]+)_)`, 'g')

/** Checks whether the provided string is a single number digit. */
export function isDigit(char: string): boolean {
    const code = char.charCodeAt(0)
    return code >= 48 && code <= 57
}

/** Checks whether the provided string is a single lowercase character. */
export function isLowerChar(char: string): boolean {
    const code = char.charCodeAt(0)
    return code >= 97 && code <= 122
}

/** Checks whether the provided string is a single uppercase character. */
export function isUpperChar(char: string): boolean {
    const code = char.charCodeAt(0)
    return code >= 65 && code <= 90
}

/** Checks whether the provided string is a single digit, lowercase character, or uppercase
 * character. */
export function isBasicChar(char: string): boolean {
    return isDigit(char) || isLowerChar(char) || isUpperChar(char)
}

/** Un-mangles the name. See docs of `mangle` to learn more. */
export function unmangle(name: string): string {
    return name.replace(NAME_REGEX, (...args) => {
        /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
        const groups: { underscore: string; specialChar: string } = args.at(-1)
        if (groups.underscore) {
            return '_'
        } else {
            return String.fromCharCode(parseInt(groups.specialChar))
        }
    })
}

/** Mangles the name. Converts all `_` to `__` and all non-ASCII characters to `_<charCode>_`. */
export function mangle(name: string): string {
    let result = ''
    for (const char of name) {
        if (isBasicChar(char)) {
            result += char
        } else if (char === '_') {
            result += '__'
        } else {
            result += `_${char.charCodeAt(0)}_`
        }
    }
    return result
}
