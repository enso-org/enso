/** @file This module defines paths within the client distribution's resources. */

/** Distribution directory for IDE. */
export function getIdeDirectory(): string {
  return requireEnv('ENSO_BUILD_IDE')
}

/** Path to the backend bundle root. */
export function getBackendBundlePath(): string {
  return requireEnv('ENSO_BUILD_BACKEND')
}

/** Get the environment variable value, assert that it is set. */
function requireEnv(name: string) {
  const value = process.env[name]
  if (value == null) {
    throw new Error(`Could not find the environment variable '${name}'.`)
  } else {
    return value
  }
}
