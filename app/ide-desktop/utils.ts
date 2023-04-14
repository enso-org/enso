/** @file Shared utility functions. */
import * as fs from 'node:fs'
import * as path from 'node:path'
import process from 'node:process'

// =================
// === Constants ===
// =================

/** Indent size for outputting JSON. */
export const INDENT_SIZE = 4

// ===================
// === Environment ===
// ===================

/**
 * Get the environment variable value.
 *
 * @param name - The name of the environment variable.
 * @returns The value of the environment variable.
 * @throws {Error} If the environment variable is not set.
 */
export function requireEnv(name: string) {
    return (
        process.env[name] ??
        (() => {
            throw Error(`Missing ${name} environment variable.`)
        })()
    )
}

/**
 * Read the path from environment variable and resolve it.
 *
 * @param name - The name of the environment variable.
 * @returns The resolved path.
 * @throws {Error} If the environment variable is not set.
 */
export function requireEnvResolvedPath(name: string) {
    return path.resolve(requireEnv(name))
}

/**
 * Read the path from environment variable and resolve it. Verify that it exists.
 *
 * @param name - The name of the environment variable.
 * @returns The resolved path.
 * @throws {Error} If the environment variable is not set or path does not exist.
 */
export function requireEnvPathExist(name: string) {
    const value = requireEnv(name)
    if (fs.existsSync(value)) {
        return value
    } else {
        throw Error(`File with path ${value} read from environment variable ${name} is missing.`)
    }
}

// ======================
// === String Helpers ===
// ======================

/** Get the common prefix of the two strings. */
export function getCommonPrefix(a: string, b: string): string {
    let i = 0
    while (i < a.length && i < b.length && a[i] === b[i]) {
        i++
    }
    return a.slice(0, i)
}
