export const MAIN_ENTRY_POINT_PREFIX = 'entry_point_'
export const BEFORE_MAIN_ENTRY_POINT_PREFIX = 'before_main_entry_point_'
export const NAME_REGEX = new RegExp(String.raw`(?<underscore>__)|(_(?<specialChar>[0-9]+)_)`, 'g')

export class EntryPoint {
    prefix: string
    name: string
    private constructor(prefix: string, name: string) {
        this.prefix = prefix
        this.name = name
    }

    fullName(): string {
        return this.prefix + this.name
    }

    displayName(): string {
        return this.name.replace(NAME_REGEX, (...args) => {
            let groups = args.at(-1)
            if (groups.underscore) {
                return '_'
            } else {
                return String.fromCharCode(parseInt(groups.specialChar))
            }
        })
    }

    static tryAs(prefix: string, fullName: string): EntryPoint | null {
        if (fullName.startsWith(prefix)) {
            return new EntryPoint(prefix, fullName.substring(prefix.length))
        } else {
            return null
        }
    }

    static tryAsMainEntryPoint(fullName: string): EntryPoint | null {
        return EntryPoint.tryAs(MAIN_ENTRY_POINT_PREFIX, fullName)
    }

    static tryAsBeforeMainEntryPoint(fullName: string): EntryPoint | null {
        return EntryPoint.tryAs(BEFORE_MAIN_ENTRY_POINT_PREFIX, fullName)
    }

    static entryPoints(prefix: string, names: string[]): Map<string, EntryPoint> {
        return names
            .map(n => EntryPoint.tryAs(prefix, n))
            .filter((n): n is EntryPoint => n != null)
            .reduce((map, n) => {
                map.set(n.name, n)
                return map
            }, new Map())
    }

    static mainEntryPoints(names: string[]): Map<string, EntryPoint> {
        return EntryPoint.entryPoints(MAIN_ENTRY_POINT_PREFIX, names)
    }

    static beforeMainEntryPoints(names: string[]): Map<string, EntryPoint> {
        return EntryPoint.entryPoints(BEFORE_MAIN_ENTRY_POINT_PREFIX, names)
    }
}

export function wasmFunctions(wasm: any): string[] {
    let names = Object.getOwnPropertyNames(wasm)
    names.sort()
    return names
}
