/**
 * @file This module contains functions for importing projects into the Project Manager.
 *
 * Eventually this module should be replaced with a new Project Manager API that supports
 * importing projects.
 * For now, we basically do the following:
 * - if the project is already in the Project Manager's location, we just open it;
 * - if the project is in a different location, we copy it to the Project Manager's location
 * and open it.
 * - if the project is a bundle, we extract it to the Project Manager's location and open it.
 */
import * as crypto from 'node:crypto'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as pathModule from 'node:path'
import type * as stream from 'node:stream'

import * as tar from 'tar'

import * as common from 'enso-common'
import * as buildUtils from 'enso-common/src/buildUtils'

import * as desktopEnvironment from '@/desktopEnvironment'

const logger = console

// =================
// === Constants ===
// =================

export const PACKAGE_METADATA_RELATIVE_PATH = 'package.yaml'
export const PROJECT_METADATA_RELATIVE_PATH = '.enso/project.json'
/** The filename suffix for the project bundle, including the leading period character. */
const BUNDLED_PROJECT_SUFFIX = '.enso-project'

// ===================
// === ProjectInfo ===
// ===================

/** Metadata for a newly imported project. */
export interface ProjectInfo {
  readonly id: string
  readonly name: string
  readonly parentDirectory: string
}

// ======================
// === Project Import ===
// ======================

/**
 * Open a project from the given path. Path can be either a source file under the project root,
 * or the project bundle. If needed, the project will be imported into the Project Manager-enabled
 * location.
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 * @throws {Error} if the path does not belong to a valid project.
 */
export function importProjectFromPath(
  openedPath: string,
  directory?: string | null,
  name: string | null = null,
) {
  directory ??= getProjectsDirectory()
  if (pathModule.extname(openedPath).endsWith(BUNDLED_PROJECT_SUFFIX)) {
    logger.log(`Path '${openedPath}' denotes a bundled project.`)
    // The second part of condition is for the case when someone names a directory
    // like `my-project.enso-project` and stores the project there.
    // Not the most fortunate move, but...
    if (isProjectRoot(openedPath)) {
      return importDirectory(openedPath, directory, name)
    } else {
      // Project bundle was provided, so we need to extract it first.
      return importBundle(openedPath, directory, name)
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
      return importDirectory(rootPath, directory, name)
    }
  }
}

/**
 * Import the project from a bundle.
 * @returns Project ID (from Project Manager's metadata) identifying the imported project.
 */
export function importBundle(
  bundlePath: string,
  directory?: string | null,
  name: string | null = null,
) {
  directory ??= getProjectsDirectory()
  logger.log(`Importing project '${bundlePath}' from bundle${name != null ? ` as '${name}'` : ''}.`)
  // The bundle is a tarball, so we just need to extract it to the right location.
  const bundlePrefix = prefixInBundle(bundlePath)
  // We care about spurious '.' and '..' when stripping paths but not when generating name.
  const normalizedBundlePrefix =
    bundlePrefix != null ?
      pathModule.normalize(bundlePrefix).replace(/[\\/]+$/, '') // Also strip trailing slash.
    : null
  const dirNameBase =
    (
      normalizedBundlePrefix != null &&
      normalizedBundlePrefix !== '.' &&
      normalizedBundlePrefix !== '..'
    ) ?
      normalizedBundlePrefix
    : bundlePath
  logger.log(`Bundle normalized prefix: '${String(normalizedBundlePrefix)}'.`)
  const targetPath = generateDirectoryName(name ?? dirNameBase, directory)
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
  return bumpMetadata(targetPath, directory, name ?? null)
}

/** Upload the project from a bundle. */
export async function uploadBundle(
  bundle: stream.Readable,
  directory?: string | null,
  name: string | null = null,
) {
  directory ??= getProjectsDirectory()
  logger.log(`Uploading project from bundle${name != null ? ` as '${name}'` : ''}.`)

  const targetPath = generateDirectoryName(name ?? 'Project', directory)
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
      const temporaryDirectoryName = targetPath + `_${crypto.randomUUID().split('-')[0] ?? ''}`
      fs.renameSync(targetPath, temporaryDirectoryName)
      fs.renameSync(pathModule.join(temporaryDirectoryName, firstEntry), targetPath)
      fs.rmdirSync(temporaryDirectoryName)
    }
  }
  return bumpMetadata(targetPath, directory, name ?? null)
}

/**
 * Import the project so it becomes visible to the Project Manager.
 * @returns The project ID (from the Project Manager's metadata) identifying the imported project.
 * @throws {Error} if a race condition occurs when generating a unique project directory name.
 */
export function importDirectory(
  rootPath: string,
  directory?: string | null,
  name: string | null = null,
): ProjectInfo {
  directory ??= getProjectsDirectory()
  if (isProjectInstalled(rootPath, directory)) {
    // Project is already visible to Project Manager, so we can just return its ID.
    logger.log(`Project already installed at '${rootPath}'.`)
    const id = getProjectId(rootPath)
    if (id != null) {
      return { id, name: getPackageName(rootPath) ?? '', parentDirectory: directory }
    } else {
      throw new Error(`Project already installed, but missing metadata.`)
    }
  } else {
    logger.log(`Importing a project copy from '${rootPath}'${name != null ? ` as '${name}'` : ''}.`)
    const targetPath = generateDirectoryName(rootPath, directory)
    logger.log(`Copying: '${rootPath}' -> '${targetPath}'.`)
    fs.cpSync(rootPath, targetPath, { recursive: true, force: true })
    // Update the project ID, so we are certain that it is unique.
    // This would be violated, if we imported the same project multiple times.
    return bumpMetadata(targetPath, directory, name ?? null)
  }
}

// ================
// === Metadata ===
// ================

/** The Project Manager's metadata associated with a project. */
interface ProjectMetadata {
  /**
   * The ID of the project. It is only used in communication with project manager;
   * it has no semantic meaning.
   */
  readonly id: string
  /** The project variant. This is currently always `UserProject`. */
  readonly kind: 'UserProject'
  /** The date at which the project was created, in RFC3339 format. */
  readonly created: string
  /** The date at which the project was last opened, in RFC3339 format. */
  readonly lastOpened: string
}

/**
 * A type guard function to check if an object conforms to the {@link ProjectMetadata} interface.
 *
 * This function checks if the input object has the required properties and correct types
 * to match the {@link ProjectMetadata} interface. It can be used at runtime to validate that
 * a given object has the expected shape.
 * @param value - The object to check against the ProjectMetadata interface.
 * @returns A boolean value indicating whether the object matches
 * the {@link ProjectMetadata} interface.
 */
function isProjectMetadata(value: unknown): value is ProjectMetadata {
  return typeof value === 'object' && value != null && 'id' in value && typeof value.id === 'string'
}

/** Get the ID from the project metadata. */
export function getProjectId(projectRoot: string): string | null {
  return getMetadata(projectRoot)?.id ?? null
}

/** Get the package name. */
function getPackageName(projectRoot: string) {
  const path = pathModule.join(projectRoot, PACKAGE_METADATA_RELATIVE_PATH)
  const contents = fs.readFileSync(path, { encoding: 'utf-8' })
  const [, name] = contents.match(/^name: (.*)/) ?? []
  return name ?? null
}

/** Update the package name. */
export function updatePackageName(projectRoot: string, name: string) {
  const path = pathModule.join(projectRoot, PACKAGE_METADATA_RELATIVE_PATH)
  const contents = fs.readFileSync(path, { encoding: 'utf-8' })
  const newContents = contents.replace(/^name: .*/, `name: ${name}`)
  fs.writeFileSync(path, newContents)
}

/** Create a project's metadata. */
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
  const metadataPath = pathModule.join(projectRoot, PROJECT_METADATA_RELATIVE_PATH)
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
  const metadataPath = pathModule.join(projectRoot, PROJECT_METADATA_RELATIVE_PATH)
  fs.mkdirSync(pathModule.dirname(metadataPath), { recursive: true })
  fs.writeFileSync(metadataPath, JSON.stringify(metadata, null, buildUtils.INDENT_SIZE))
}

/**
 * Update the project's metadata.
 * If the provided updater does not return anything, the metadata file is left intact.
 *
 * Returns the metadata returned from the updater function.
 */
export function updateMetadata(
  projectRoot: string,
  updater: (initialMetadata: ProjectMetadata) => ProjectMetadata,
): ProjectMetadata {
  const metadata = getMetadata(projectRoot)
  const updatedMetadata = updater(metadata ?? createMetadata())
  writeMetadata(projectRoot, updatedMetadata)
  return updatedMetadata
}

// =========================
// === Project Directory ===
// =========================

/**
 * Check if the given path represents the root of an Enso project.
 * This is decided by the presence of the Project Manager's metadata.
 */
export function isProjectRoot(candidatePath: string): boolean {
  const projectJsonPath = pathModule.join(candidatePath, PROJECT_METADATA_RELATIVE_PATH)
  try {
    fs.accessSync(projectJsonPath, fs.constants.R_OK)
    return true
  } catch {
    return false
  }
}

/**
 * Check if this bundle is a compressed directory (rather than directly containing the project
 * files). If it is, we return the path to the directory. Otherwise, we return `null`.
 */
export function prefixInBundle(bundlePath: string): string | null {
  // We need to look up the root directory among the tarball entries.
  let commonPrefix: string | null = null
  tar.list({
    file: bundlePath,
    sync: true,
    onentry: entry => {
      const path = entry.path
      commonPrefix = commonPrefix == null ? path : buildUtils.getCommonPrefix(commonPrefix, path)
    },
  })

  return commonPrefix != null && commonPrefix !== '' ? commonPrefix : null
}

/**
 * Generate a name for a project using given base string. A suffix is added if there is a
 * collision.
 *
 * For example `Name` will become `Name_1` if there's already a directory named `Name`.
 * If given a name like `Name_1` it will become `Name_2` if there is already a directory named
 * `Name_1`. If a path containing multiple components is given, only the last component is used
 * for the name.
 */
export function generateDirectoryName(name: string, directory = getProjectsDirectory()): string {
  // Use only the last path component.
  name = pathModule.parse(name).name

  // If the name already consists a suffix, reuse it.
  const matches = name.match(/^(.*)_(\d+)$/)
  // Matches start with the whole match, so we need to skip it. Then come our two capture groups.
  const [matchedName, matchedSuffix] = matches?.slice(1) ?? []

  if (typeof matchedName !== 'undefined' && typeof matchedSuffix !== 'undefined') {
    name = matchedName
  }

  return pathModule.join(directory, name)
}

/**
 * Take a path to a file, presumably located in a project's subtree.Returns the path
 * to the project's root directory or `null` if the file is not located in a project.
 */
export function getProjectRoot(subtreePath: string): string | null {
  let currentPath = subtreePath
  while (!isProjectRoot(currentPath)) {
    const parent = pathModule.dirname(currentPath)
    if (parent === currentPath) {
      return null
    }
    currentPath = parent
  }
  return currentPath
}

/** Get the directory that stores Enso projects. */
export function getProjectsDirectory(): string {
  if (process.env.ENSO_TEST != null && process.env.ENSO_TEST !== '') {
    return pathModule.join(os.tmpdir(), 'enso-test-projects', process.env.ENSO_TEST)
  } else {
    const documentsPath = desktopEnvironment.DOCUMENTS
    if (documentsPath === undefined) {
      return pathModule.join(os.homedir(), 'enso', 'projects')
    } else {
      return pathModule.join(documentsPath, 'enso-projects')
    }
  }
}

/** Check if the given project is installed, i.e. can be opened with the Project Manager. */
export function isProjectInstalled(
  projectRoot: string,
  directory = getProjectsDirectory(),
): boolean {
  const projectRootParent = pathModule.dirname(projectRoot)
  // Should resolve symlinks and relative paths. Normalize before comparison.
  return pathModule.resolve(projectRootParent) === pathModule.resolve(directory)
}

// ==================
// === Project ID ===
// ==================

/** Generate a unique UUID for a project. */
export function generateId(): string {
  return crypto.randomUUID()
}

/** Update the project's ID to a new, unique value, and its last opened date to the current date. */
export function bumpMetadata(
  projectRoot: string,
  parentDirectory: string,
  name: string | null,
): ProjectInfo {
  if (name == null) {
    const currentName = getPackageName(projectRoot) ?? ''
    let index: number | null = null
    const prefix = `${currentName} `
    for (const sibling of fs.readdirSync(parentDirectory, { withFileTypes: true })) {
      const siblingPath = pathModule.join(parentDirectory, sibling.name)
      if (siblingPath === projectRoot) {
        continue
      } else if (sibling.isDirectory()) {
        try {
          const siblingName = getPackageName(siblingPath)
          if (siblingName === currentName) {
            index = index ?? 2
          } else if (siblingName != null && siblingName.startsWith(prefix)) {
            const suffix = siblingName.replace(prefix, '')
            const [, numberString] = suffix.match(/^\((\d+)\)/) ?? []
            if (numberString != null) {
              index = Math.max(index ?? 2, Number(numberString) + 1)
            }
          }
        } catch {
          // Ignored - it is a directory but not a project.
        }
      }
    }
    name = index == null ? currentName : `${currentName} (${index})`
  }
  updatePackageName(projectRoot, name)
  const id = updateMetadata(projectRoot, metadata => ({
    ...metadata,
    id: generateId(),
    lastOpened: new Date().toISOString(),
  })).id
  return { id, name, parentDirectory }
}
