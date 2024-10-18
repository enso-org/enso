/** @file Shared utility functions. */

/** Indent size for stringifying JSON. */
export const INDENT_SIZE: number

/**
 * Get the environment variable value.
 * @param name - The name of the environment variable.
 * @returns The value of the environment variable.
 * @throws {Error} If the environment variable is not set.
 */
export function requireEnv(name: string): string

/**
 * Read the path from environment variable and resolve it.
 * @param name - The name of the environment variable.
 * @returns The resolved path.
 * @throws {Error} If the environment variable is not set.
 */
export function requireEnvResolvedPath(name: string): string

/**
 * Read the path from environment variable and resolve it. Verify that it exists.
 * @param name - The name of the environment variable.
 * @returns The resolved path.
 * @throws {Error} If the environment variable is not set or path does not exist.
 */
export function requireEnvPathExist(name: string): string

/**
 * Get the common prefix of the two strings.
 * @param a - the first string.
 * @param b - the second string.
 * @returns The common prefix.
 */
export function getCommonPrefix(a: string, b: string): string
