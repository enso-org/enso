/** @file File system paths used by the application. */

import path from 'node:path'
import { project_manager_bundle } from '../paths'
import Electron from 'electron'

// =============
// === Paths ===
// =============

/** The root of the application bundle. */
export const app = Electron.app.getAppPath()

/** The application assets, all files bundled with it. */
export const assets = path.join(app, 'assets')

/** Other app resources, including binaries, such a project manager. */
export const resources = path.join(app, '..')

/** Project manager binary path. */
export const projectManager = path.join(
    resources,
    project_manager_bundle,
    // @ts-ignore
    // Placeholder for a bundler-provided define.
    PROJECT_MANAGER_IN_BUNDLE_PATH
)
