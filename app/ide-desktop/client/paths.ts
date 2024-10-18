/** @file This module defines paths within the client distribution's resources. */

import * as buildUtils from 'enso-common/src/buildUtils'

// ==========================
// === Paths to resources ===
// ==========================

/**
 * Path to the Project Manager bundle within the electron distribution
 * (relative to electron's resources directory).
 */
export const PROJECT_MANAGER_BUNDLE = 'enso'

/** Distribution directory for IDE. */
export function getIdeDirectory(): string {
  return buildUtils.requireEnv('ENSO_BUILD_IDE')
}

/** Path to the project manager bundle root. */
export function getProjectManagerBundlePath(): string {
  return buildUtils.requireEnv('ENSO_BUILD_PROJECT_MANAGER')
}

/** Path to the project manager executable relative to the PM bundle root. */
export function getProjectManagerInBundlePath(): string {
  return buildUtils.requireEnv('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')
}
