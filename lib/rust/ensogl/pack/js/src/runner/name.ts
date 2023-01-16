export const NAME_REGEX = new RegExp(String.raw`(?<underscore>__)|(_(?<specialChar>[0-9]+)_)`, 'g')

export function isDigit(char: string): boolean {
    const code = char.charCodeAt(0)
    return code >= 48 && code <= 57
}

export function isLowerChar(char: string): boolean {
    const code = char.charCodeAt(0)
    return code >= 97 && code <= 122
}

export function isUpperChar(char: string): boolean {
    const code = char.charCodeAt(0)
    return code >= 65 && code <= 90
}

export function isBasicChar(char: string): boolean {
    return isDigit(char) || isLowerChar(char) || isUpperChar(char)
}

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
