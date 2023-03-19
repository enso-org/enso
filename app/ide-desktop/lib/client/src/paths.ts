/** @file File system paths used by the application. */

import path from 'node:path'
import { project_manager_bundle } from '../paths'
import Electron from 'electron'
import isDev from 'electron-is-dev'
import fss from "node:fs";

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

/** Relative path of Enso Project PM metadata relative to project's root. */
export const projectMetadataRelative = path.join('.enso', 'project.json')

/** Get the arguments, excluding the initial program name and any electron dev mode arguments. */
export const clientArguments = isDev ? process.argv.slice(process.argv.indexOf('--') + 1) : process.argv.slice(1)

    /** Check if the given path represents the root of an Enso project. This is decided by the presence of Project Manager's metadata.
    */
export function isProjectRoot(candidatePath: string): boolean {
    let project_json_path = path.join(candidatePath, projectMetadataRelative)
    try {
        fss.accessSync(project_json_path, fss.constants.R_OK)
        return true
    } catch (e) {
        return false
    }
}

/** Get the ID from the project metadata. */
export function getProjectId(projectRoot: string): string {
    let metadataPath = path.join(projectRoot, projectMetadataRelative)
    let jsonText = fss.readFileSync(metadataPath, 'utf8')
    let parsedMetadata = JSON.parse(jsonText)
    return parsedMetadata.id
}

/** Get the project root path from its subtree. */
export function getProjectRoot(subtreePath: string): string {
    let currentPath = subtreePath
    while (!isProjectRoot(currentPath)) {
        currentPath = path.dirname(currentPath)
    }
    return currentPath
}
