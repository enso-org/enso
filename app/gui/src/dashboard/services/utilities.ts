/**
 * @file Module containing utility functions related to any backend.
 */
import FolderIcon from '#/assets/folder.svg'
import type { AnyCategory } from '../layouts/Drive/Categories/Category'
import type { DirectoryId } from './Backend'
import { Path } from './Backend'
import { DIRECTORY_ID_PREFIX, newDirectoryId } from './LocalBackend'
/**
 * Options for the parseDirectoriesPath function.
 */
export interface ParsedDirectoriesPathOptions {
  readonly rootDirectoryId: DirectoryId
  readonly getCategoryByDirectoryId: (id: DirectoryId) => AnyCategory | null
  readonly parentsPath: string
  readonly virtualParentsPath: string
}

/** An item in the path. */
export interface PathItem {
  readonly id: DirectoryId
  readonly categoryId: AnyCategory['id'] | null
  readonly label: AnyCategory['label']
  readonly icon: AnyCategory['icon']
}

/**
 * Parse the parents path and virtual parents path into a list of {@link PathItem}.
 */
export function parseDirectoriesPath(options: ParsedDirectoriesPathOptions) {
  const { getCategoryByDirectoryId, parentsPath, rootDirectoryId, virtualParentsPath } = options

  // Split the path using the helper function
  // Parents path is a string of directory ids separated by slashes, but the ids are not escaped, so we need to split them manually.
  // e.g: parentsPath = 'directory-id1adsf/directory-id2adsf/directory-id3adsf'
  const splitPath = splitDirectoryPath(parentsPath)
  const rootDirectoryInPath = splitPath[0] ?? rootDirectoryId

  const splitVirtualParentsPath = virtualParentsPath.split('/')
  // Virtual parents path is a string of directory names separated by slashes.
  // To match the ids with the names, we need to remove the first element of the split path.
  // As the first element is the root directory, which is not a virtual parent.
  // e.g:
  // assume directory-id1adsf - the root directory(cloud/local)
  // virtualParentsPath = 'parent1/parent2', splitVirtualParentsPath = ['parent1', 'parent2']
  // parentsPath = 'directory-id1adsf/directory-id2adsf/directory-id3adsf', splitPath = ['directory-id1adsf', 'directory-id2adsf', 'directory-id3adsf']
  // We remove the root directory from the split path (it doesn't exist in the virtual parents path) -> virtualParentsIds = ['directory-id2adsf', 'directory-id3adsf']
  const virtualParentsIds = splitPath.slice(1)

  const finalPath = (() => {
    const result: PathItem[] = []

    const rootCategory = getCategoryByDirectoryId(rootDirectoryInPath)

    // If the root category is not found it might mean
    // that user is no longer have access to this root directory.
    // Usually this could happen if the user was removed from the organization
    // or user group.
    // This shouldn't happen though and these files should be filtered out
    // by the backend. But we need to handle this case anyway.
    if (rootCategory == null) {
      return result
    }

    result.push({
      id: rootDirectoryId,
      icon: rootCategory.icon,
      label: rootCategory.label,
      categoryId: rootCategory.id,
    })

    for (const [index, id] of virtualParentsIds.entries()) {
      const name = splitVirtualParentsPath.at(index)

      if (name == null) {
        continue
      }

      result.push({
        id,
        label: name,
        icon: FolderIcon,
        categoryId: rootCategory.id,
      })
    }

    return result
  })()

  return { finalPath } as const
}

/**
 * Splits a path string containing directory IDs into an array of individual directory IDs.
 * Handles cases where directory IDs themselves may contain forward slashes.
 */
function splitDirectoryPath(path: string): DirectoryId[] {
  if (path === '') {
    return []
  }

  const result: DirectoryId[] = []

  const directories = path.split(DIRECTORY_ID_PREFIX)

  // Iterate through each character
  for (const directory of directories) {
    if (directory === '') {
      continue
    }

    result.push(newDirectoryId(Path(directory.replace(/\/$/, ''))))
  }

  return result
}
