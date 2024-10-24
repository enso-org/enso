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

/** Get the environment variable value.
 * @param {string} name - The name of the environment variable.
 * @returns {string} The value of the environment variable.
 * @throws {Error} If the environment variable is not set. */
export function requireEnv(name) {
  const value = process.env[name]
  if (value == null) {
    throw new Error(`Could not find the environment variable '${name}'.`)
  } else {
    return value
  }
}

/** Read the path from environment variable and resolve it.
 * @param {string} name - The name of the environment variable.
 * @returns {string} The resolved path.
 * @throws {Error} If the environment variable is not set. */
export function requireEnvResolvedPath(name) {
  return path.resolve(requireEnv(name))
}

/** Read the path from environment variable and resolve it. Verify that it exists.
 * @param {string} name - The name of the environment variable.
 * @returns {string} The resolved path.
 * @throws {Error} If the environment variable is not set or path does not exist. */
export function requireEnvPathExist(name) {
  const value = requireEnv(name)
  if (fs.existsSync(value)) {
    return value
  } else {
    throw Error(
      `Could not find the file at '${value}' defined by the environment variable '${name}'.`,
    )
  }
}

// ======================
// === String Helpers ===
// ======================

/** Get the common prefix of the two strings.
 * @param {string} a - the first string.
 * @param {string} b - the second string.
 * @returns {string} The common prefix. */
export function getCommonPrefix(a, b) {
  let i = 0
  while (i < a.length && i < b.length && a[i] === b[i]) {
    i++
  }
  return a.slice(0, i)
}
