/** @file This module defines paths within the client distribution's resources. */

import * as utils from '../../utils'

// ==========================
// === Paths to resources ===
// ==========================

/** Path to the Project Manager bundle within the electron distribution
 * (relative to electron's resources directory). */
export const PROJECT_MANAGER_BUNDLE = 'enso'

/** Distribution directory for IDE. */
export function getIdeDirectory(): string {
    return utils.requireEnv('ENSO_BUILD_IDE')
}

/** Distribution directory for GUI. */
export function getGuiDirectory(): string {
    return utils.requireEnv('ENSO_BUILD_GUI')
}

/** Path to the project manager bundle root. */
export function getProjectManagerBundlePath(): string {
    return utils.requireEnv('ENSO_BUILD_PROJECT_MANAGER')
}

/** Path to the project manager executable relative to the PM bundle root. */
export function getProjectManagerInBundlePath(): string {
    return utils.requireEnv('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')
}

/** Version of the Engine (backend) that is bundled along with this client build. */
export function getBundledEngineVersion(): string {
    return utils.requireEnv('ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION')
}
