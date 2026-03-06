/** @file Backend agnostic utility functions. */
import type { DirectoryId } from '../Backend.js'
import type { AnyCategory } from './Category.js'

/** Options for {@link parseDirectoriesPath}. */
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

/** Parse the parents path and virtual parents path into a list of {@link PathItem}. */
export function parseDirectoriesPath(options: ParsedDirectoriesPathOptions) {
  const { getCategoryByDirectoryId, parentsPath, rootDirectoryId, virtualParentsPath } = options

  // e.g: parentsPath = 'directory-id1adsf/directory-id2adsf/directory-id3adsf'

  const splitPath = parentsPath.split('/') as DirectoryId[]
  const rootDirectoryInPath = splitPath[0] || rootDirectoryId

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

  const response: {
    readonly finalPath: readonly PathItem[]
    readonly category: AnyCategory | null
  } = (() => {
    const result: PathItem[] = []

    const rootCategory = getCategoryByDirectoryId(rootDirectoryInPath)

    // If the root category is not found it might mean that the user no longer has access
    // to this root directory.
    // Usually this could happen if the user was removed from the organization or user group.
    // This shouldn't happen though and these files should be filtered out by the backend.
    // But we need to handle this case anyway.
    if (rootCategory == null) {
      return { finalPath: [], category: null }
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
        icon: 'folder',
        categoryId: rootCategory.id,
      })
    }

    return { finalPath: result, category: rootCategory }
  })()

  return response
}
