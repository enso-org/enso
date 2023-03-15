/** @file File system paths used by the application. */

import * as path from 'node:path'

import * as electron from 'electron'
import electronIsDev from 'electron-is-dev'

import * as paths from '../paths'

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
