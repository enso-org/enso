/** @file This module contains functions for importing projects into the Project Manager.
 *
 * Eventually this module should be replaced with a new Project Manager API that supports
 * importing projects.
 * For now, we basically do the following:
 * - if the project is already in the Project Manager's location, we just open it;
 * - if the project is in a different location, we copy it to the Project Manager's location
 * and open it.
 * - if the project is a bundle, we extract it to the Project Manager's location and open it. */

import * as crypto from 'node:crypto'
import * as fs from 'node:fs'
import * as pathModule from 'node:path'
import type * as stream from 'node:stream'

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

/** Open a project from the given path. Path can be either a source file under the project root,
 * or the project bundle. If needed, the project will be imported into the Project Manager-enabled
 * location.
 *
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 * @throws {Error} if the path does not belong to a valid project. */
export function importProjectFromPath(openedPath: string): string {
    if (pathModule.extname(openedPath).endsWith(fileAssociations.BUNDLED_PROJECT_SUFFIX)) {
        logger.log(`Path '${openedPath}' denotes a bundled project.`)
        // The second part of condition is for the case when someone names a directory
        // like `my-project.enso-project` and stores the project there.
        // Not the most fortunate move, but...
        if (isProjectRoot(openedPath)) {
            return importDirectory(openedPath)
        } else {
            // Project bundle was provided, so we need to extract it first.
            return importBundle(openedPath)
        }
    } else {
        logger.log(`Opening non-bundled file: '${openedPath}'.`)
        const rootPath = getProjectRoot(openedPath)
        // Check if the project root is under the projects directory. If it is, we can open it.
        // Otherwise, we need to install it first.
        if (rootPath == null) {
            const productName = common.PRODUCT_NAME
            const message = `File '${openedPath}' does not belong to the ${productName} project.`
            throw new Error(message)
        } else {
            return importDirectory(rootPath)
        }
    }
}

/** Import the project from a bundle.
 *
 * @returns Project ID (from Project Manager's metadata) identifying the imported project. */
export function importBundle(bundlePath: string): string {
    logger.log(`Importing project '${bundlePath}' from bundle.`)
    // The bundle is a tarball, so we just need to extract it to the right location.
    const bundlePrefix = prefixInBundle(bundlePath)
    // We care about spurious '.' and '..' when stripping paths but not when generating name.
    const normalizedBundlePrefix =
        bundlePrefix != null
            ? pathModule.normalize(bundlePrefix).replace(/[\\/]+$/, '') // Also strip trailing slash.
            : null
    const dirNameBase =
        normalizedBundlePrefix != null &&
        normalizedBundlePrefix !== '.' &&
        normalizedBundlePrefix !== '..'
            ? normalizedBundlePrefix
            : bundlePath
    logger.log(`Bundle normalized prefix: '${String(normalizedBundlePrefix)}'.`)
    const targetPath = generateDirectoryName(dirNameBase)
    logger.log(`Importing project as '${targetPath}'.`)
    fs.mkdirSync(targetPath, { recursive: true })
    // To be more resilient against different ways that user might attempt to create a bundle,
    // we try to support both archives that:
    // * contain a single directory with the project files - that directory name will be used
    //   to generate a new target directory name;
    // * contain the project files directly - in this case, the archive filename will be used
    //   to generate a new target directory name.
    // We try to tell apart these two cases by looking at the common prefix of the paths
    // of the files in the archive. If there is any, everything is under a single directory,
    // and we need to strip it.
    //
    // Additionally, we need to take into account that paths might be prefixed with `./` or not.
    // Thus, we need to adjust the number of path components to strip accordingly.

    logger.log(`Extracting bundle: '${bundlePath}' -> '${targetPath}'.`)

    // Strip trailing separator and split the path into pieces.
    const rootPieces = bundlePrefix != null ? bundlePrefix.split(/[\\/]/) : []

    // If the last element is empty string (i.e. we had trailing separator), drop it.
    if (rootPieces.length > 0 && rootPieces[rootPieces.length - 1] === '') {
        rootPieces.pop()
    }

    tar.extract({
        file: bundlePath,
        cwd: targetPath,
        sync: true,
        strip: rootPieces.length,
    })
    return updateIdAndDate(targetPath)
}

/** Upload the project from a bundle. */
export async function uploadBundle(bundle: stream.Readable): Promise<string> {
    logger.log(`Uploading project from bundle.`)
    const targetPath = generateDirectoryName('Project')
    fs.mkdirSync(targetPath, { recursive: true })
    await new Promise<void>(resolve => {
        bundle.pipe(tar.extract({ cwd: targetPath })).on('finish', resolve)
    })
    const entries = fs.readdirSync(targetPath)
    const firstEntry = entries[0]
    // If the directory only contains one subdirectory, replace the directory with its sole
    // subdirectory.
    if (entries.length === 1 && firstEntry != null) {
        if (fs.statSync(pathModule.join(targetPath, firstEntry)).isDirectory()) {
            const temporaryDirectoryName =
                targetPath + `_${crypto.randomUUID().split('-')[0] ?? ''}`
            fs.renameSync(targetPath, temporaryDirectoryName)
            fs.renameSync(pathModule.join(temporaryDirectoryName, firstEntry), targetPath)
            fs.rmdirSync(temporaryDirectoryName)
        }
    }
    return updateIdAndDate(targetPath)
}

/** Import the project so it becomes visible to the Project Manager.
 *
 * @param rootPath - The path to the project root.
 * @returns The project ID (from the Project Manager's metadata) identifying the imported project.
 * @throws {Error} if a race condition occurs when generating a unique project directory name. */
export function importDirectory(rootPath: string): string {
    if (isProjectInstalled(rootPath)) {
        // Project is already visible to Project Manager, so we can just return its ID.
        logger.log(`Project already installed at '${rootPath}'.`)
        const id = getProjectId(rootPath)
        if (id != null) {
            return id
        } else {
            throw new Error(`Project already installed, but missing metadata.`)
        }
    } else {
        logger.log(`Importing a project copy from '${rootPath}'.`)
        const targetPath = generateDirectoryName(rootPath)
        if (fs.existsSync(targetPath)) {
            throw new Error(`Project directory '${targetPath}' already exists.`)
        } else {
            logger.log(`Copying: '${rootPath}' -> '${targetPath}'.`)
            fs.cpSync(rootPath, targetPath, { recursive: true })
            // Update the project ID, so we are certain that it is unique.
            // This would be violated, if we imported the same project multiple times.
            return updateIdAndDate(targetPath)
        }
    }
}

// ================
// === Metadata ===
// ================

/** The Project Manager's metadata associated with a project. */
interface ProjectMetadata {
    /** The ID of the project. It is only used in communication with project manager;
     * it has no semantic meaning. */
    id: string
    /** The project variant. This is currently always `UserProject`. */
    kind: 'UserProject'
    /** The date at which the project was created, in RFC3339 format. */
    created: string
    /** The date at which the project was last opened, in RFC3339 format. */
    lastOpened: string
}

/** A type guard function to check if an object conforms to the {@link ProjectMetadata} interface.
 *
 * This function checks if the input object has the required properties and correct types
 * to match the {@link ProjectMetadata} interface. It can be used at runtime to validate that
 * a given object has the expected shape.
 *
 * @param value - The object to check against the ProjectMetadata interface.
 * @returns A boolean value indicating whether the object matches
 * the {@link ProjectMetadata} interface. */
function isProjectMetadata(value: unknown): value is ProjectMetadata {
    return (
        typeof value === 'object' && value != null && 'id' in value && typeof value.id === 'string'
    )
}

/** Get the ID from the project metadata. */
export function getProjectId(projectRoot: string): string | null {
    return getMetadata(projectRoot)?.id ?? null
}

/** Create  */
export function createMetadata(): ProjectMetadata {
    return {
        id: generateId(),
        kind: 'UserProject',
        created: new Date().toISOString(),
        lastOpened: new Date().toISOString(),
    }
}

/** Retrieve the project's metadata. */
export function getMetadata(projectRoot: string): ProjectMetadata | null {
    const metadataPath = pathModule.join(projectRoot, paths.PROJECT_METADATA_RELATIVE)
    try {
        const jsonText = fs.readFileSync(metadataPath, 'utf8')
        const metadata: unknown = JSON.parse(jsonText)
        return isProjectMetadata(metadata) ? metadata : null
    } catch {
        return null
    }
}

/** Write the project's metadata. */
export function writeMetadata(projectRoot: string, metadata: ProjectMetadata): void {
    const metadataPath = pathModule.join(projectRoot, paths.PROJECT_METADATA_RELATIVE)
    fs.mkdirSync(pathModule.dirname(metadataPath), { recursive: true })
    fs.writeFileSync(metadataPath, JSON.stringify(metadata, null, utils.INDENT_SIZE))
}

/** Update the project's metadata.
 * If the provided updater does not return anything, the metadata file is left intact.
 *
 * Returns the metadata returned from the updater function. */
export function updateMetadata(
    projectRoot: string,
    updater: (initialMetadata: ProjectMetadata) => ProjectMetadata
): ProjectMetadata {
    const metadata = getMetadata(projectRoot)
    const updatedMetadata = updater(metadata ?? createMetadata())
    writeMetadata(projectRoot, updatedMetadata)
    return updatedMetadata
}

// =========================
// === Project Directory ===
// =========================

/** Check if the given path represents the root of an Enso project.
 * This is decided by the presence of the Project Manager's metadata. */
export function isProjectRoot(candidatePath: string): boolean {
    const projectJsonPath = pathModule.join(candidatePath, paths.PROJECT_METADATA_RELATIVE)
    try {
        fs.accessSync(projectJsonPath, fs.constants.R_OK)
        return true
    } catch {
        return false
    }
}

/** Check if this bundle is a compressed directory (rather than directly containing the project
 * files). If it is, we return the path to the directory. Otherwise, we return `null`. */
export function prefixInBundle(bundlePath: string): string | null {
    // We need to look up the root directory among the tarball entries.
    let commonPrefix: string | null = null
    tar.list({
        file: bundlePath,
        sync: true,
        onentry: entry => {
            const path = entry.path
            commonPrefix = commonPrefix == null ? path : utils.getCommonPrefix(commonPrefix, path)
        },
    })

    // ESLint doesn't know that `commonPrefix` can be not `null` here due to the `onentry` callback.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    return commonPrefix != null && commonPrefix !== '' ? commonPrefix : null
}

/** Generate a name for a project using given base string. A suffix is added if there is a
 * collision.
 *
 * For example `Name` will become `Name_1` if there's already a directory named `Name`.
 * If given a name like `Name_1` it will become `Name_2` if there is already a directory named
 * `Name_1`. If a path containing multiple components is given, only the last component is used
 * for the name. */
export function generateDirectoryName(name: string): string {
    // Use only the last path component.
    name = pathModule.parse(name).name

    // If the name already consists a suffix, reuse it.
    const matches = name.match(/^(.*)_(\d+)$/)
    const initialSuffix = -1
    let suffix = initialSuffix
    // Matches start with the whole match, so we need to skip it. Then come our two capture groups.
    const [matchedName, matchedSuffix] = matches?.slice(1) ?? []
    if (typeof matchedName !== 'undefined' && typeof matchedSuffix !== 'undefined') {
        name = matchedName
        suffix = parseInt(matchedSuffix)
    }

    const projectsDirectory = getProjectsDirectory()
    let finalPath: string
    while (true) {
        suffix++
        const newName = `${name}${suffix === 0 ? '' : `_${suffix}`}`
        const candidatePath = pathModule.join(projectsDirectory, newName)
        if (!fs.existsSync(candidatePath)) {
            finalPath = candidatePath
            break
        }
    }
    return finalPath
}

/** Take a path to a file, presumably located in a project's subtree.Returns the path
 * to the project's root directory or `null` if the file is not located in a project. */
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
    // Project can be opened by project manager only if its root directory is directly under
    // the projects directory.
    const projectsDirectory = getProjectsDirectory()
    const projectRootParent = pathModule.dirname(projectRoot)
    // Should resolve symlinks and relative paths. Normalize before comparison.
    return pathModule.resolve(projectRootParent) === pathModule.resolve(projectsDirectory)
}

// ==================
// === Project ID ===
// ==================

/** Generate a unique UUID for a project. */
export function generateId(): string {
    return crypto.randomUUID()
}

/** Update the project's ID to a new, unique value, and its last opened date to the current date. */
export function updateIdAndDate(projectRoot: string): string {
    return updateMetadata(projectRoot, metadata => ({
        ...metadata,
        id: generateId(),
        lastOpened: new Date().toISOString(),
    })).id
}
