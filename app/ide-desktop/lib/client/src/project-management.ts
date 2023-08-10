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
import * as stream from 'node:stream'

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

/** Information required to display a project bundle. */
export interface BundleInfo {
    name: string
    id: string
}

/** Open a project from the given path. Path can be either a source file under the project root,
 * or the project bundle. If needed, the project will be imported into the Project Manager-enabled
 * location.
 *
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 * @throws {Error} if the path does not belong to a valid project. */
export function importProjectFromPath(openedPath: string): BundleInfo {
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
export function importBundle(bundlePath: string): BundleInfo {
    logger.log(`Importing project '${bundlePath}' from bundle.`)
    // The bundle is a tarball, so we just need to extract it to the right location.
    const bundleRoot = directoryWithinBundle(bundlePath)
    const target = generateDirectoryName(bundleRoot ?? bundlePath)
    logger.log(`Importing project as '${target.name}'.`)
    fs.mkdirSync(target.path, { recursive: true })
    // To be more resilient against different ways that user might attempt to create a bundle,
    // we try to support both archives that:
    // * contain a single directory with the project files - that directory name will be used
    //   to generate a new target directory name;
    // * contain the project files directly - in this case, the archive filename will be used
    //   to generate a new target directory name.
    // We try to tell apart these two cases by looking at the common prefix of the paths
    // of the files in the archive. If there is any, everything is under a single directory,
    // and we need to strip it.
    tar.extract({
        file: bundlePath,
        cwd: target.path,
        sync: true,
        strip: bundleRoot != null ? 1 : 0,
    })
    updateName(target.path, target.name)
    return { name: target.name, id: updateIdAndDate(target.path) }
}

/** Upload the project from a bundle. */
export async function uploadBundle(bundle: stream.Readable): Promise<BundleInfo> {
    logger.log(`Uploading project from bundle.`)
    let target = generateDirectoryName('Project')
    fs.mkdirSync(target.path, { recursive: true })
    await new Promise<void>(resolve => {
        bundle.pipe(tar.extract({ cwd: target.path })).on('finish', resolve)
    })
    const entries = fs.readdirSync(target.path)
    const firstEntry = entries[0]
    // If the directory only contains one subdirectory, replace the directory with its sole
    // subdirectory.
    if (entries.length === 1 && firstEntry != null) {
        if (fs.statSync(pathModule.join(target.path, firstEntry)).isDirectory()) {
            const temporaryDirectoryName =
                target.path + `_${crypto.randomUUID().split('-')[0] ?? ''}`
            fs.renameSync(target.path, temporaryDirectoryName)
            fs.renameSync(pathModule.join(temporaryDirectoryName, firstEntry), target.path)
            fs.rmdirSync(temporaryDirectoryName)
        }
    }
    const projectName = tryGetName(target.path)
    if (projectName != null) {
        const oldPath = target.path
        target = generateDirectoryName(projectName)
        if (target.path !== oldPath) {
            fs.renameSync(oldPath, target.path)
        }
    }
    updateName(target.path, target.name)
    return { name: target.name, id: updateIdAndDate(target.path) }
}

/** Import the project so it becomes visible to the Project Manager.
 *
 * @param rootPath - The path to the project root.
 * @returns The project ID (from the Project Manager's metadata) identifying the imported project.
 * @throws {Error} if a race condition occurs when generating a unique project directory name. */
export function importDirectory(rootPath: string): BundleInfo {
    if (isProjectInstalled(rootPath)) {
        // Project is already visible to Project Manager, so we can just return its ID.
        logger.log(`Project already installed at '${rootPath}'.`)
        const id = getProjectId(rootPath)
        if (id != null) {
            return { name: getProjectName(rootPath), id }
        } else {
            throw new Error(`Project already installed, but missing metadata.`)
        }
    } else {
        logger.log(`Importing a project copy from '${rootPath}'.`)
        const target = generateDirectoryName(rootPath)
        if (fs.existsSync(target.path)) {
            throw new Error(`Project directory '${target.path}' already exists.`)
        } else {
            logger.log(`Copying: '${rootPath}' -> '${target.path}'.`)
            fs.cpSync(rootPath, target.path, { recursive: true })
            updateName(target.path, target.name)
            // Update the project ID, so we are certain that it is unique.
            // This would be violated, if we imported the same project multiple times.
            return { name: target.name, id: updateIdAndDate(target.path) }
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
    const packageYamlPath = pathModule.join(candidatePath, paths.BUNDLE_METADATA_RELATIVE)
    const projectJsonPath = pathModule.join(candidatePath, paths.PROJECT_METADATA_RELATIVE)
    let isRoot = false
    try {
        fs.accessSync(packageYamlPath, fs.constants.R_OK)
    } catch {
        try {
            fs.accessSync(projectJsonPath, fs.constants.R_OK)
            isRoot = true
        } catch {
            // No need to do anything, isRoot is already set to false
        }
    }
    return isRoot
}

/** Check if this bundle is a compressed directory (rather than directly containing the project
 * files). If it is, we return the name of the directory. Otherwise, we return `null`. */
export function directoryWithinBundle(bundlePath: string): string | null {
    // We need to look up the root directory among the tarball entries.
    let commonPrefix: string | null = null
    tar.list({
        file: bundlePath,
        sync: true,
        onentry: entry => {
            // We normalize to get rid of leading `.` (if any).
            const path = entry.path.normalize()
            commonPrefix = commonPrefix == null ? path : utils.getCommonPrefix(commonPrefix, path)
        },
    })
    // ESLint doesn't know that `commonPrefix` can be not `null` here due to the `onentry` callback.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    return commonPrefix != null && commonPrefix !== '' ? pathModule.basename(commonPrefix) : null
}

/** An object containing the name and path of a project. */
interface NameAndPath {
    name: string
    path: string
}

/** Generate a name for a project using given base string. A suffix is added if there is a
 * collision.
 *
 * For example `Name` will become `Name_1` if there's already a directory named `Name`.
 * If given a name like `Name_1` it will become `Name_2` if there is already a directory named
 * `Name_1`. If a path containing multiple components is given, only the last component is used
 * for the name. */
export function generateDirectoryName(name: string): NameAndPath {
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
    while (true) {
        suffix++
        const newName = `${name}${suffix === 0 ? '' : `_${suffix}`}`
        const candidatePath = pathModule.join(projectsDirectory, newName)
        if (!fs.existsSync(candidatePath)) {
            // eslint-disable-next-line no-restricted-syntax
            return { name: newName, path: candidatePath }
        }
    }
    // Unreachable.
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

/** Get the name of the project from the bundle metadata (`package.yaml`). */
export function getProjectName(projectRoot: string) {
    const metadataPath = pathModule.join(projectRoot, paths.BUNDLE_METADATA_RELATIVE)
    const metadata = fs.readFileSync(metadataPath, 'utf-8')
    return metadata.match(/^name: (.+)$/)?.[1] ?? ''
}

/** Update the name of the project in the bundle metadata (`package.yaml`). */
export function updateName(projectRoot: string, newName: string) {
    const metadataPath = pathModule.join(projectRoot, paths.BUNDLE_METADATA_RELATIVE)
    const oldMetadata = fs.readFileSync(metadataPath, 'utf-8')
    const newMetadata = oldMetadata.replace(/^name: .+$/m, `name: ${newName}`)
    fs.writeFileSync(metadataPath, newMetadata)
}

/** Gets the name of a bundle from the bundle metadata (`package.yaml`). */
export function tryGetName(projectRoot: string) {
    const metadataPath = pathModule.join(projectRoot, paths.BUNDLE_METADATA_RELATIVE)
    try {
        const metadata = fs.readFileSync(metadataPath, 'utf-8')
        return metadata.match(/^name: (.+)$/m)?.[1] ?? null
    } catch {
        /// The bundle metadata file was not found.
        return null
    }
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
