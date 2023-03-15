/** @file utility file */
import path from 'node:path'
import fs from 'node:fs'
import process from 'node:process'

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
    if (fs.existsSync(value)) return value
    else throw Error(`File with path ${value} read from environment variable ${name} is missing.`)
}

/**
 * Function fulfills after the given path denotes an existing, readable file.
 */
async function waitUntilReadable(path: string) {
    // This implementation (polling every 100ms) is crude but should be reliable.
    // If such need arises, more refined implementation can be built using `fs.watch` api.
    console.log(`Waiting for file ${path} to become readable.`)
    for (;;) {
        try {
            await fs.promises.access(path, fs.constants.R_OK)
            return
        } catch (err) {
            await new Promise(resolve => setTimeout(resolve, 100))
        }
    }
}

// FIXME[sb]: why are some functions not exported?
export default { requireEnv, requireEnvPathExist }
