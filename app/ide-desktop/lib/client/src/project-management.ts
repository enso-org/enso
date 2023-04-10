/** @file This module contains functions for importing projects into the Project Manager.
 *
 * Eventually this module should be replaced with a new Project Manager API that supports importing projects.
 * For now, we basically do the following:
 * - if the project is already in the Project Manager's location, we just open it;
 * - if the project is in a different location, we copy it to the Project Manager's location and open it.
 * - if the project is a bundle, we extract it to the Project Manager's location and open it.
 */

import * as crypto from 'node:crypto'
import * as fsSync from 'node:fs'
import * as fss from 'node:fs'
import * as pathModule from 'node:path'

import * as electron from 'electron'
import * as tar from 'tar'

import * as common from 'enso-common'
import * as config from 'enso-content-config'
import * as fileAssociations from '../file-associations'
import * as paths from './paths'
import * as utils from '../../../utils'

const logger = config.logger

// ======================
// === Project Import ===
// ======================

/** Open a project from the given path. Path can be either a source file under the project root, or the project
 * bundle. If needed, the project will be imported into the Project Manager-enabled location.
 *
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 * @throws `Error` if the path does not belong to a valid project.
 */
export function importProjectFromPath(openedPath: string): string {
    if (pathModule.extname(openedPath).endsWith(fileAssociations.BUNDLED_PROJECT_EXTENSION)) {
        // The second part of condition is for the case when someone names a directory like `my-project.enso-project`
        // and stores the project there. Not the most fortunate move, but...
        if (isProjectRoot(openedPath)) {
            return importDirectory(openedPath)
        } else {
            // Project bundle was provided, so we need to extract it first.
            return importBundle(openedPath)
        }
    } else {
        logger.log(`Opening file: '${openedPath}'.`)
        const rootPath = getProjectRoot(openedPath)
        // Check if the project root is under the projects directory. If it is, we can open it.
        // Otherwise, we need to install it first.
        if (rootPath == null) {
            const message = `File '${openedPath}' does not belong to the ${common.PRODUCT_NAME} project.`
            throw new Error(message)
        }
        return importDirectory(rootPath)
    }
}

/** Import the project from a bundle.
 *
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 */
export function importBundle(bundlePath: string): string {
    // The bundle is a tarball, so we just need to extract it to the right location.
    const bundleRoot = directoryWithinBundle(bundlePath)
    const targetDirectory = generateDirectoryName(bundleRoot ?? bundlePath)
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
        strip: bundleRoot != null ? 1 : 0,
    })
    return updateId(targetDirectory)
}

/** Import the project, so it becomes visible to Project Manager.
 *
 * @param rootPath - The path to the project root.
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 * @throws `Error` if there occurs race-condition when generating a unique project directory name.
 */
export function importDirectory(rootPath: string): string {
    if (isProjectInstalled(rootPath)) {
        // Project is already visible to Project Manager, so we can just return its ID.
        logger.log(`Project already installed: '${rootPath}'.`)
        return getProjectId(rootPath)
    } else {
        logger.log(`Importing a project copy from: '${rootPath}'.`)
        const targetDirectory = generateDirectoryName(rootPath)
        if (fsSync.existsSync(targetDirectory)) {
            const message = `Project directory already exists: ${targetDirectory}.`
            throw new Error(message)
        }

        logger.log(`Copying: '${rootPath}' -> '${targetDirectory}'.`)
        fsSync.cpSync(rootPath, targetDirectory, { recursive: true })
        // Update the project ID, so we are certain that it is unique. This would be violated, if we imported the same
        // project multiple times.
        return updateId(targetDirectory)
    }
}

// ================
// === Metadata ===
// ================

/** The Project Manager's metadata associated with a project.
 *
 * The property list is not exhaustive, it only contains the properties that we need.
 */
interface ProjectMetadata {
    /** The ID of the project. It is only used in communication with project manager, it has no semantic meaning. */
    id: string
}

/**
 * Type guard function to check if an object conforms to the ProjectMetadata interface.
 *
 * This function checks if the input object has the required properties and correct types
 * to match the ProjectMetadata interface. It can be used at runtime to validate that
 * a given object has the expected shape.
 *
 * @param value - The object to check against the ProjectMetadata interface.
 * @returns A boolean value indicating whether the object matches the ProjectMetadata interface.
 */
function isProjectMetadata(value: unknown): value is ProjectMetadata {
    return (
        typeof value === 'object' && value != null && 'id' in value && typeof value.id === 'string'
    )
}

/** Get the ID from the project metadata. */
export function getProjectId(projectRoot: string): string {
    return getMetadata(projectRoot).id
}

/** Retrieve the project's metadata.
 *
 * @throws `Error` if the metadata file is missing or ill-formed. */
export function getMetadata(projectRoot: string): ProjectMetadata {
    const metadataPath = pathModule.join(projectRoot, paths.PROJECT_METADATA_RELATIVE)
    const jsonText = fss.readFileSync(metadataPath, 'utf8')
    const metadata: unknown = JSON.parse(jsonText)
    if (isProjectMetadata(metadata)) {
        return metadata
    } else {
        throw new Error('Invalid project metadata')
    }
}

/** Write the project's metadata. */
export function writeMetadata(projectRoot: string, metadata: ProjectMetadata): void {
    const metadataPath = pathModule.join(projectRoot, paths.PROJECT_METADATA_RELATIVE)
    fss.writeFileSync(metadataPath, JSON.stringify(metadata, null, utils.INDENT_SIZE))
}

/** Update project's metadata. If the provided updater does not return anything, the metadata file is left intact.
 *
 * The updater function-returned metadata is passed over.
 */
export function updateMetadata(
    projectRoot: string,
    updater: (initialMetadata: ProjectMetadata) => ProjectMetadata
): ProjectMetadata {
    const metadata = getMetadata(projectRoot)
    const updatedMetadata = updater(metadata)
    writeMetadata(projectRoot, updatedMetadata)
    return updatedMetadata
}

// =========================
// === Project Directory ===
// =========================

/* Check if the given path represents the root of an Enso project. This is decided by the presence
 * of Project Manager's metadata. */
export function isProjectRoot(candidatePath: string): boolean {
    const projectJsonPath = pathModule.join(candidatePath, paths.PROJECT_METADATA_RELATIVE)
    let isRoot = false
    try {
        fss.accessSync(projectJsonPath, fss.constants.R_OK)
        isRoot = true
    } catch (e) {
        // No need to do anything, isRoot is already set to false
    }
    return isRoot
}

/** Check if this bundle is a compressed directory (rather than directly containing the project files). If it is, we
 * return the name of the directory. Otherwise, we return `null`. */
export function directoryWithinBundle(bundlePath: string): string | null {
    // We need to look up the root directory among the tarball entries.
    let commonPrefix: string | null = null
    tar.list({
        file: bundlePath,
        sync: true,
        onentry: entry => {
            // We normalize to get rid of leading `.` (if any).
            let path = entry.path.normalize()
            commonPrefix = commonPrefix == null ? path : utils.getCommonPrefix(commonPrefix, path)
        },
    })
    // ESLint doesn't understand that `commonPrefix` can be not `null` here due to the `onentry` callback.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    return commonPrefix ? pathModule.basename(commonPrefix) : null
}

/** Generate a name for project using given base string. Suffixes are added if there's a collision.
 *
 * For example 'Name' will become 'Name_1' if there's already a directory named 'Name'.
 * If given a name like 'Name_1' it will become 'Name_2' if there's already a directory named 'Name_1'.
 * If a path containing multiple components is given, only the last component is used for the name. */
export function generateDirectoryName(name: string): string {
    // Use only the last path component.
    name = pathModule.parse(name).name

    // If the name already consists a suffix, reuse it.
    const matches = name.match(/^(.*)_(\d+)$/)
    let suffix = 0
    // Matches start with the whole match, so we need to skip it. Then come our two capture groups.
    const [matchedName, matchedSuffix] = matches?.slice(1) ?? []
    if (typeof matchedName !== 'undefined' && typeof matchedSuffix !== 'undefined') {
        name = matchedName
        suffix = parseInt(matchedSuffix)
    }

    const projectsDirectory = getProjectsDirectory()
    for (; ; suffix++) {
        let candidatePath = pathModule.join(
            projectsDirectory,
            `${name}${suffix === 0 ? '' : `_${suffix}`}`
        )
        if (!fss.existsSync(candidatePath)) {
            // eslint-disable-next-line no-restricted-syntax
            return candidatePath
        }
    }
    // Unreachable.
}

/** Takes a path to a file, presumably located in a project's subtree. Returns the path to the project's root directory
 * or `null` if the file is not located in a project. */
export function getProjectRoot(subtreePath: string): string | null {
    let currentPath = subtreePath
    while (!isProjectRoot(currentPath)) {
        const parent = pathModule.dirname(currentPath)
        if (parent === currentPath) {
            // eslint-disable-next-line no-restricted-syntax
            return null
        }
        currentPath = parent
    }
    return currentPath
}

/** Get the directory that stores Enso projects. */
export function getProjectsDirectory(): string {
    return pathModule.join(electron.app.getPath('home'), 'enso', 'projects')
}

/** Check if the given project is installed, i.e. can be opened with the Project Manager. */
export function isProjectInstalled(projectRoot: string): boolean {
    // Project can be opened by project manager only if its root directory is directly under the projects directory.
    const projectsDirectory = getProjectsDirectory()
    const projectRootParent = pathModule.dirname(projectRoot)
    // Should resolve symlinks and relative paths. Normalize before comparison.
    return pathModule.resolve(projectRootParent) === pathModule.resolve(projectsDirectory)
}

// ==================
// === Project ID ===
// ==================

/** Generates a unique UUID for a project. */
export function generateId(): string {
    return crypto.randomUUID()
}

/** Update the project's ID to a new, unique value. */
export function updateId(projectRoot: string): string {
    return updateMetadata(projectRoot, metadata => ({
        ...metadata,
        id: generateId(),
    })).id
}
