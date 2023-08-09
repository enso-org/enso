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
 * - for development `…` (just the directory with `index.js`). */
export const APP_PATH = electron.app.getAppPath()

/** The path of the directory in which the log files of IDE are stored.
 *
 * This is based on the Electron `logs` directory, see {@link Electron.App.getPath}. */
export const LOGS_DIRECTORY = electron.app.getPath('logs')

/** The application assets, all files bundled with it. */
export const ASSETS_PATH = path.join(APP_PATH, 'assets')

/** Path to the `resources` folder.
 *
 * Contains other app resources, including binaries, such a project manager. */
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
/** Relative path of Enso Project bundle metadata relative to project's root. */
export const BUNDLE_METADATA_RELATIVE = path.join('package.yaml')
