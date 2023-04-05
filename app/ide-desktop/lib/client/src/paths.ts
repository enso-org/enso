/** @file File system paths used by the application. */

import * as path from 'node:path'

import * as electron from 'electron'
import electronIsDev from 'electron-is-dev'

import * as paths from '../paths.js'

// =============
// === Paths ===
// =============

/** The root of the application bundle.
 *
 * This path is like:
 * - for packaged application `…/resources/app.asar`;
 * - for development `…` (just the directory with `index.js`).
 */
export const APP_PATH = electron.app.getAppPath()

/** The application assets, all files bundled with it. */
export const ASSETS_PATH = path.join(APP_PATH, 'assets')

/** Path to the `resources` folder.
 *
 * Contains other app resources, including binaries, such a project manager.
 */
export const RESOURCES_PATH = electronIsDev ? APP_PATH : path.join(APP_PATH, '..')

/** Project manager binary path. */
export const PROJECT_MANAGER_PATH = path.join(
    RESOURCES_PATH,
    paths.PROJECT_MANAGER_BUNDLE,
    // Placeholder for a bundler-provided define.
    PROJECT_MANAGER_IN_BUNDLE_PATH
)

/** Relative path of Enso Project PM metadata relative to project's root. */
export const PROJECT_METADATA_RELATIVE = path.join('.enso', 'project.json')

/** Get the arguments, excluding the initial program name and any electron dev mode arguments. */
export const CLIENT_ARGUMENTS = getClientArguments()

/** Decide what are client arguments, @see {@link CLIENT_ARGUMENTS}. */
function getClientArguments(): string[] {
    if (electronIsDev) {
        // Client arguments are separated from the electron dev mode arguments by a '--' argument.
        const separator = '--'
        const separatorIndex = process.argv.indexOf(separator)
        const notFoundIndexPlaceholder = -1
        if (separatorIndex === notFoundIndexPlaceholder) {
            // If there is no separator, client gets no arguments.
            return []
        } else {
            // Drop everything before the separator.
            return process.argv.slice(separatorIndex + 1)
        }
    } else {
        // Drop the leading executable name.
        return process.argv.slice(1)
    }
}
