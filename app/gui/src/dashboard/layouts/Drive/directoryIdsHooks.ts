/** @file A hook returning the root directory id and expanded directory ids. */
import type { Category } from '$/providers/category'
import { useCategories, useUser } from '$/providers/react'
import { useDriveCurrentDirectory } from '$/providers/react/container'

/** Options for {@link useDirectoryIds}. */
export interface UseDirectoryIdsOptions {
  readonly category: Category
}

/** A hook returning the root directory id and expanded directory ids. */
export function useDirectoryIds(options: UseDirectoryIdsOptions) {
  const { category } = options

  const { categoryDirectoryId } = useCategories()
  const user = useUser()
  const categoryHomeDir = categoryDirectoryId(category)

  const rootDirectoryId = categoryHomeDir ?? user.rootDirectoryId
  /** The id of the directory to use in the "list directory" query. */
  const queryDirectoryId = useDriveCurrentDirectory()[0] ?? categoryHomeDir
  const currentDirectoryId = queryDirectoryId ?? rootDirectoryId

  return {
    rootDirectoryId,
    queryDirectoryId,
    currentDirectoryId,
  } as const
}
