/** @file File system paths used by the application. */

import path from 'node:path'
import { project_manager_bundle } from '../paths'
import Electron from 'electron'
import isDev from 'electron-is-dev'

// =============
// === Paths ===
// =============

/** The root of the application bundle.
 *
 * This path is like:
 *  - for packaged application `…/resources/app.asar`;
 *  - for development `…` (just the directory with `index.js`).
 **/
export const app = Electron.app.getAppPath()

/** The application assets, all files bundled with it. */
export const assets = path.join(app, 'assets')

/** Path to the `resources` folder.
 *
 * Contains other app resources, including binaries, such a project manager.
 **/
export const resources = isDev ? app : path.join(app, '..')

/** Project manager binary path. */
export const projectManager = path.join(
    resources,
    project_manager_bundle,
    // @ts-ignore
    // Placeholder for a bundler-provided define.
    PROJECT_MANAGER_IN_BUNDLE_PATH
)
