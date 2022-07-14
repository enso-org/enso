import path from 'node:path'
import process from 'node:process'

/**
 * Get the environment variable value.
 *
 * @param {string} name The name of the environment variable.
 * @returns {string} The value of the environment variable.
 * @throws {Error} If the environment variable is not set.
 */
export function require_env(name) {
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
 * @param {string} name The name of the environment variable.
 * @returns {string} The resolved path.
 * @throws {Error} If the environment variable is not set.
 */
export function require_env_resolved_path(name) {
    return path.resolve(require_env(name))
}

/**
 * Read the path from environment variable and resolve it. Verify that it exists.
 *
 * @param {string} name The name of the environment variable.
 * @returns {string} The resolved path.
 * @throws {Error} If the environment variable is not set or path does not exist.
 */
export function require_env_path_exist(name) {
    const value = require_env(name)
    if (path.existsSync(value)) return value
    else throw Error(`File with path ${value} read from environment variable ${name} is missing.`)
}

export default { require_env, require_env_path_exist }
