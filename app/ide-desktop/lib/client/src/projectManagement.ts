/** @file This module contains functions for importing projects into the Project Manager.
 *
 * Eventually this module should be replaced with a new Project Manager API that supports importing projects.
 * For now, we basically do the following:
 * - if the project is already in the Project Manager's location, we just open it;
 * - if the project is in a different location, we copy it to the Project Manager's location and open it.
 * - if the project is a bundle, we extract it to the Project Manager's location and open it.
 **/

import path from 'node:path'
import fss from 'node:fs'
import electron from 'electron'
import crypto from 'node:crypto'

import { isProjectRoot, projectMetadataRelative } from './paths'
import fsSync from 'node:fs'
import { BUNDLED_PROJECT_EXTENSION } from '../paths'
import * as tar from 'tar'

interface ProjectMetadata {
    id: string
}

/** Open a project from the given path. Path can be either a source file under the project root, or the project
 * bundle. If needed, the project will be imported into the Project Manager-enabled location.
 *
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 */
export function openFile(openedPath: string): string {
    if (path.extname(openedPath).endsWith(BUNDLED_PROJECT_EXTENSION)) {
        // The second part of condition is for the case when someone names a directory like `my-project.enso-project`
        // and stores the project there. Not the most fortunate move, but...
        if (isProjectRoot(openedPath)) {
            return importDirectory(openedPath)
        } else {
            // Project bundle was provided, so we need to extract it first.
            return importBundle(openedPath)
        }
    } else {
        console.log(`Opening file: ${openedPath}`)
        const rootPath = getProjectRoot(openedPath)
        // Check if the project root is under the projects directory. If it is, we can open it.
        // Otherwise, we need to install it first.
        if (rootPath === null) {
            // Perhaps bundle?
            let message = `File '${openedPath}' does not belong to the Enso project.`
            throw new Error(message)
        }
        return importDirectory(rootPath)
    }
}

/** Get the common prefix of the two strings. */
export function getCommonPrefix(a: string, b: string): string {
    let i = 0
    while (i < a.length && i < b.length && a[i] === b[i]) {
        i++
    }
    return a.slice(0, i)
}

/** If the bundle consists of a single directory, return its name. Otherwise, return null. */
export function bundleRootDirectory(bundlePath: string): string | null {
    // We need to look up the root directory among the tarball entries.
    let commonPrefix: string | null = null
    tar.list({
        file: bundlePath,
        sync: true,
        onentry: entry => {
            // We normalize to get rid of leading `.` (if any).
            let path = entry.path.normalize()
            commonPrefix = commonPrefix === null ? path : getCommonPrefix(commonPrefix, path)
            console.log(`Entry: ${entry.path}, common prefix: ${commonPrefix}`)
        },
    })
    if (commonPrefix !== null && commonPrefix !== '') {
        return path.basename(commonPrefix)
    } else {
        return null
    }
}

/** Import the project from a bundle.

 @returns Project ID (from Project Manager's metadata) identifying the imported project.
 */
export function importBundle(bundlePath: string): string {
    // The bundle is a tarball, so we just need to extract it to the right location.
    const bundleRoot = bundleRootDirectory(bundlePath)
    const targetDirectory = bundleRoot
        ? generateDirectoryName(bundleRoot)
        : generateDirectoryName(bundlePath)
    fss.mkdirSync(targetDirectory, { recursive: true })
    // To be more resilient against different ways that user might attempt to create a bundle, we try to support
    // both archives that:
    // * contain a single directory with the project files - that directory name will be used to generate a new target
    //   directory name;
    // * contain the project files directly - in this case, the archive filename will be used to generate a new target
    //   directory name.
    // We try to tell apart these two cases by looking at the common prefix of the paths of the files in the archive.
    // If there is any, everything is under a single directory, and we need to strip it.
    tar.x({
        file: bundlePath,
        cwd: targetDirectory,
        sync: true,
        strip: bundleRoot !== null ? 1 : 0,
    })
    return updateId(targetDirectory)
}

/** Import the project, so it becomes visible to Project Manager.
 *
 * @param rootPath The path to the project root.
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 **/
export function importDirectory(rootPath: string): string {
    if (isProjectInstalled(rootPath)) {
        // Project is already visible to Project Manager, so we can just return its ID.
        console.debug(`Project already installed: ${rootPath}`)
        return getProjectId(rootPath)
    }

    console.debug(`Importing a project copy from: ${rootPath}`)
    const targetDirectory = generateDirectoryName(rootPath)
    if (fsSync.existsSync(targetDirectory)) {
        const message = `Project directory already exists: ${targetDirectory}.`
        throw new Error(message)
    }

    console.debug(`Copying: ${rootPath} -> ${targetDirectory}`)
    fsSync.cpSync(rootPath, targetDirectory, { recursive: true })
    // Update the project ID, so we are certain that it is unique. This would be violated, if we imported the same
    // project multiple times.
    return updateId(targetDirectory)
}

/** Generate a name for project using given base string. Suffixes are added if there's a collision.
 *
 * For example 'Name' will become 'Name_1' if there's already a directory named 'Name'.
 * If given a name like 'Name_1' it will become 'Name_2' if there's already a directory named 'Name_1'.
 * If a path containing multiple components is given, only the last component is used for the name.
 **/
export function generateDirectoryName(name: string): string {
    // Use only the last path component.
    name = path.parse(name).name

    // If the name already consists a suffix, reuse it.
    let match = name.match(/^(.*)_(\d+)$/)
    let suffix = 0
    if (match !== null && match.length === 3) {
        name = match[1] as string
        suffix = parseInt(match[2] as string) as number
    }

    let projectsDirectory = getProjectsDirectory()
    for (; ; suffix++) {
        let candidatePath = path.join(
            projectsDirectory,
            `${name}${suffix === 0 ? '' : '_' + suffix}`
        )
        console.log(`Checking if ${candidatePath} exists?`)
        if (!fss.existsSync(candidatePath)) {
            return candidatePath
        }
    }
    // Unreachable.
}

/** Get the ID from the project metadata. */
export function getProjectId(projectRoot: string): string {
    return getMetadata(projectRoot).id
}

export function getMetadata(projectRoot: string): ProjectMetadata {
    const metadataPath = path.join(projectRoot, projectMetadataRelative)
    const jsonText = fss.readFileSync(metadataPath, 'utf8')
    return JSON.parse(jsonText)
}

export function writeMetadata(projectRoot: string, metadata: ProjectMetadata): void {
    updateMetadata(projectRoot, () => metadata)
}

/** Update project's metadata. If the provided updater does not return anything, the metadata file is left intact.
 *
 *  The updater function-returned metadata is passed over.
 **/
export function updateMetadata<R>(projectRoot: string, updater: (a: ProjectMetadata) => R): R {
    const metadataPath = path.join(projectRoot, projectMetadataRelative)
    const jsonText = fss.readFileSync(metadataPath, 'utf8')
    const parsedMetadata = JSON.parse(jsonText)
    const updatedMetadata = updater(parsedMetadata)
    if (updatedMetadata !== undefined) {
        fss.writeFileSync(metadataPath, JSON.stringify(updatedMetadata, null, 4))
    }
    return updatedMetadata
}

/** Get the project root path from its subtree. */
export function getProjectRoot(subtreePath: string): string | null {
    console.debug(`Getting project root for ${subtreePath}.`)
    let currentPath = subtreePath
    while (!isProjectRoot(currentPath)) {
        const parent = path.dirname(currentPath)
        if (parent === currentPath) {
            return null
        }
        currentPath = parent
    }
    return currentPath
}

/** Get the directory that stores Enso projects. */
export function getProjectsDirectory(): string {
    return path.join(electron.app.getPath('home'), 'enso', 'projects')
}

/** Check if the given project is installed, i.e. can be opened with the Project Manager. */
export function isProjectInstalled(projectRoot: string): boolean {
    // Project can be opened by project manager only if its root directory is directly under the projects directory.
    let projectsDirectory = getProjectsDirectory()
    let projectRootParent = path.dirname(projectRoot)
    // Should resolve symlinks and relative paths. Normalize before comparison.
    return path.resolve(projectRootParent) === path.resolve(projectsDirectory)
}

/** Generates a unique UUID for a project. */
export function generateId(): string {
    return crypto.randomUUID()
}

export function updateId(projectRoot: string): string {
    return updateMetadata(projectRoot, metadata => ({
        ...metadata,
        id: generateId(),
    })).id
}
