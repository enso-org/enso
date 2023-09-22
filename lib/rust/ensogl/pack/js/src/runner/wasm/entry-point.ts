/** @file WASM entry point definition. An entry point is a WASM function exported from Rust that was
 * marked as an entry point. */

import * as name from '../name'
import { logger } from '../log/logger'

// =================
// === Constants ===
// =================

export const MAIN_ENTRY_POINT_PREFIX = 'entry_point_'
export const BEFORE_MAIN_ENTRY_POINT_PREFIX = 'before_main_entry_point_'

// ==================
// === EntryPoint ===
// ==================

/** A WASM entry point, a function exported by wasm-bindgen in Rust. There are two types of entry
 * points, before main entry points and main entry points. The former are meant to be all run before
 * the chosen main entry point is run. */
export class EntryPoint {
    prefix = MAIN_ENTRY_POINT_PREFIX
    constructor(public strippedName: string) {}

    /** The original name of the WASM function. */
    name(): string {
        return this.prefix + this.strippedName
    }

    /** The name of the function used to obtain documentation for this entry point. */
    docsFnName(): string {
        return `docs_of_${this.name()}`
    }

    /** Not mangled name, easy to read by the user. WASM function names exported from Rust are
     * mangled to contain the path and location information. This converts them to a form of
     * 'file/path/from/repo/root.rs:line:column'. */
    displayName(): string {
        return name.unmangle(this.strippedName)
    }

    /** Try parsing the name as the provided entry point type. If the name does not start with the
     * provided prefix, return `null`. */
    static tryFrom(fullName: string): EntryPoint | null {
        const prefix = MAIN_ENTRY_POINT_PREFIX
        if (fullName.startsWith(prefix)) {
            return new EntryPoint(fullName.substring(prefix.length))
        } else {
            return null
        }
    }

    /** Filter the provided names list and return array of valid entry points. The entry points will
     * be sorted in the lexicographical order. */
    static fromNamesAsArray(names: string[]): EntryPoint[] {
        const arr = names
            .map(n => EntryPoint.tryFrom(n))
            .filter((n): n is EntryPoint => n != null)
            .reduce((arr, n) => {
                arr.push(n)
                return arr
            }, new Array<EntryPoint>())
        arr.sort((a, b) => a.strippedName.localeCompare(b.strippedName))
        return arr
    }

    /** Filter the provided names list and return map of valid entry points. The entry points will
     * be sorted in the lexicographical order. */
    static fromNames(names: string[]): Map<string, EntryPoint> {
        const arr = EntryPoint.fromNamesAsArray(names)
        return new Map(arr.map(n => [n.strippedName, n]))
    }
}

// ============================
// === BeforeMainEntryPoint ===
// ============================

export class BeforeMainEntryPoint extends EntryPoint {
    constructor(
        strippedName: string,
        public priority: number
    ) {
        super(strippedName)
        this.prefix = BEFORE_MAIN_ENTRY_POINT_PREFIX
    }

    override name(): string {
        return `${this.prefix}${this.priority}_${this.strippedName}`
    }

    override displayName(): string {
        return `[${this.priority}] ${super.displayName()}`
    }

    static override tryFrom(fullName: string): EntryPoint | null {
        const prefix = BEFORE_MAIN_ENTRY_POINT_PREFIX
        if (fullName.startsWith(prefix)) {
            const suffix = fullName.substring(prefix.length)
            const underscoreIndex = suffix.indexOf('_')
            let splitIndex = prefix.length
            let priority = null
            if (underscoreIndex !== -1) {
                splitIndex += underscoreIndex + 1
                const newPriority = parseInt(suffix.substring(0, underscoreIndex))
                if (!isNaN(newPriority)) {
                    priority = newPriority
                }
            }
            if (priority == null) {
                logger.log(
                    `Invalid before main entry point name. ` +
                        `No priority prefix found in '${fullName}'.`
                )
                priority = 0
            }
            const strippedName = fullName.substring(splitIndex)
            return new BeforeMainEntryPoint(strippedName, priority)
        } else {
            return null
        }
    }

    static override fromNamesAsArray(names: string[]): BeforeMainEntryPoint[] {
        const arr = names
            .map(n => BeforeMainEntryPoint.tryFrom(n))
            .filter((n): n is BeforeMainEntryPoint => n != null)
            .reduce((arr, n) => {
                arr.push(n)
                return arr
            }, new Array<BeforeMainEntryPoint>())
        arr.sort((a, b) => a.strippedName.localeCompare(b.strippedName))
        return arr
    }

    static override fromNames(names: string[]): Map<string, BeforeMainEntryPoint> {
        const arr = BeforeMainEntryPoint.fromNamesAsArray(names)
        arr.sort((a, b) => a.priority - b.priority)
        return new Map(arr.map(n => [n.strippedName, n]))
    }
}

// =============
// === Utils ===
// =============

/** Return list of all WASM functions. The functions will be sorted by name in order to be sure that
 * the runtime results are always reproducible between builds. */
export function sortedWasmFunctions(wasm: any): string[] {
    const names = Object.getOwnPropertyNames(wasm)
    names.sort()
    return names
}
