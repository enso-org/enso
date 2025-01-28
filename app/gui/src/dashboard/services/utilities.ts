/**
 * @file Module containing utility functions related to any backend.
 */

import { isDirectoryId, type DirectoryId } from './Backend'

/**
 * A directory in the path.
 */
export interface ParsedDirectoriesPath {
  readonly id: DirectoryId
  readonly name: string
}

/**
 * Parse the parents path and virtual parents path into a list of directories.
 */
export function parseDirectoriesPath(parentsPath: string, virtualParentsPath: string) {
  // Parents path is a string of directory ids separated by slashes.
  const splitPath = parentsPath.split('/').filter(isDirectoryId)
  const rootDirectoryInPath = splitPath[0]
  // Virtual parents path is a string of directory names separated by slashes.
  // To match the ids with the names, we need to remove the first element of the split path.
  // As the first element is the root directory, which is not a virtual parent.
  const virtualParentsIds = splitPath.slice(1)

  const splitVirtualParentsPath = virtualParentsPath.split('/')

  const finalPath = (() => {
    const result: ParsedDirectoriesPath[] = []

    if (rootDirectoryInPath == null) {
      return result
    }

    result.push({
      id: rootDirectoryInPath,
      // TODO: Get the name of the root directory from categories.
      name: 'Root',
    })

    for (const [index, id] of virtualParentsIds.entries()) {
      const name = splitVirtualParentsPath.at(index)

      if (name == null) {
        continue
      }

      result.push({ id, name })
    }

    return result
  })()

  return { fullPath: finalPath }
}
