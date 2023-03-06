/**
 * This module defines paths within the client distribution's resources.
 *
 * This is knowledge that is common to code building the client package and the packaged code itself.
 *
 */

import { require_env } from '../../utils.js'

/** Path to the Project Manager bundle within the electron distribution (relative to the electron's resources directory). */
export const project_manager_bundle = 'enso'

/** Distribution directory for IDE. */
export function getIdeDirectory(): string {
    return require_env('ENSO_BUILD_IDE')
}

/** Distribution directory for GUI. */
export function getGuiDirectory(): string {
    return require_env('ENSO_BUILD_GUI')
}

/** Path to the project manager bundle root. */
export function getProjectManagerBundle(): string {
    return require_env('ENSO_BUILD_PROJECT_MANAGER')
}

/** Path to the project manager executable relative to the PM bundle root. */
export function getProjectManagerInBundlePath(): string {
    return require_env('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')
}

/** Version of the Engine (backend) that is bundled along with this client build. */
export function getBundledEngineVersion(): string {
    return require_env('ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION')
}
