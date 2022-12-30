/* WASM entry point definition. An entry point is a WASM function exported from Rust that was marked
as an entry point. */

// =================
// === Constants ===
// =================

export const MAIN_ENTRY_POINT_PREFIX = 'entry_point_'
export const BEFORE_MAIN_ENTRY_POINT_PREFIX = 'before_main_entry_point_'
export const NAME_REGEX = new RegExp(String.raw`(?<underscore>__)|(_(?<specialChar>[0-9]+)_)`, 'g')

// ==================
// === EntryPoint ===
// ==================

/** A WASM entry point, a function exported by wasm-bindgen in Rust. There are two types of entry
 * points, before main entry points and main entry points. The former are meant to be all run before
 * the chosen main entry point is run. */
export class EntryPoint {
    prefix: string
    strippedName: string
    private constructor(prefix: string, name: string) {
        this.prefix = prefix
        this.strippedName = name
    }

    /** The original name of the WASM function. */
    name(): string {
        return this.prefix + this.strippedName
    }

    /** Not mangled name, easy to read by the user. WASM function names exported from Rust are
     * mangled to contain the path and location information. This converts them to a form of
     * 'file/path/from/repo/root.rs:line:column'. */
    displayName(): string {
        return this.strippedName.replace(NAME_REGEX, (...args) => {
            let groups = args.at(-1)
            if (groups.underscore) {
                return '_'
            } else {
                return String.fromCharCode(parseInt(groups.specialChar))
            }
        })
    }

    /** Try parsing the name as the provided entry point type. If the name does not start with the
     * provided prefix, return `null`. */
    static tryAs(prefix: string, fullName: string): EntryPoint | null {
        if (fullName.startsWith(prefix)) {
            return new EntryPoint(prefix, fullName.substring(prefix.length))
        } else {
            return null
        }
    }

    /** Try parsing the name as the main entry point type. If the name does not start with
     * `MAIN_ENTRY_POINT_PREFIX`, return `null`. */
    static tryAsMainEntryPoint(fullName: string): EntryPoint | null {
        return EntryPoint.tryAs(MAIN_ENTRY_POINT_PREFIX, fullName)
    }

    /** Try parsing the name as the main entry point type. If the name does not start with
     * `BEFORE_MAIN_ENTRY_POINT_PREFIX`, return `null`. */
    static tryAsBeforeMainEntryPoint(fullName: string): EntryPoint | null {
        return EntryPoint.tryAs(BEFORE_MAIN_ENTRY_POINT_PREFIX, fullName)
    }

    /** Filter the provided names list and return map of valid entry points. */
    static entryPoints(prefix: string, names: string[]): Map<string, EntryPoint> {
        return names
            .map(n => EntryPoint.tryAs(prefix, n))
            .filter((n): n is EntryPoint => n != null)
            .reduce((map, n) => {
                map.set(n.strippedName, n)
                return map
            }, new Map())
    }

    /** Filter the provided names list and return map of valid main entry points. */
    static mainEntryPoints(names: string[]): Map<string, EntryPoint> {
        return EntryPoint.entryPoints(MAIN_ENTRY_POINT_PREFIX, names)
    }

    /** Filter the provided names list and return map of valid before main entry points. */
    static beforeMainEntryPoints(names: string[]): Map<string, EntryPoint> {
        return EntryPoint.entryPoints(BEFORE_MAIN_ENTRY_POINT_PREFIX, names)
    }
}

/** Return list of all WASM functions. The functions will be sorted by name in order to be sure that
 * the runtime results are always reproducible between builds. */
export function wasmFunctions(wasm: any): string[] {
    let names = Object.getOwnPropertyNames(wasm)
    names.sort()
    return names
}
