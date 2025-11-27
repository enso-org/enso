/** @file File system paths used by the application. */

import * as path from 'node:path'

import * as electron from 'electron'
import electronIsDev from 'electron-is-dev'

// =============
// === Paths ===
// =============

/**
 * The root of the application bundle.
 *
 * This path is like:
 * - for packaged application `…/resources/app.asar`;
 * - for development `…` (just the directory with `index.js`).
 */
export const APP_PATH = electron.app.getAppPath()

/**
 * The path of the directory in which the log files of IDE are stored.
 *
 * This is based on the Electron `logs` directory, see {@link electron.app.getPath}.
 * By default, it is `~/Library/Logs/enso` on Mac, and inside `userData` directory on Windows and Linux.
 */
export const LOGS_DIRECTORY = electron.app.getPath('logs')

/** The application assets, all files bundled with it. */
export const ASSETS_PATH = path.join(APP_PATH, 'assets')

/**
 * Path to the `resources` folder.
 *
 * Contains other app resources, including binaries, such a project manager.
 */
export const RESOURCES_PATH = electronIsDev ? APP_PATH : path.join(APP_PATH, '..')

/** Relative path of Enso Project package metadata relative to the project root. */
export const PACKAGE_METADATA_RELATIVE = 'package.yaml'
/** Relative path of Enso Project PM metadata relative to the project root. */
export const PROJECT_METADATA_RELATIVE = path.join('.enso', 'project.json')
