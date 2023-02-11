/** File system paths used by the application. */

import path from 'node:path'
import { project_manager_bundle } from '../paths.js'
import Electron from 'electron'

// =============
// === Paths ===
// =============

export const app = Electron.app.getAppPath()
export const resources = path.join(app, '..')
export const projectManager = path.join(
    resources,
    project_manager_bundle,
    // @ts-ignore
    // Placeholder for a bundler-provided define.
    PROJECT_MANAGER_IN_BUNDLE_PATH
)
