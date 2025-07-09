/** @file A hook returning the root directory id and expanded directory ids. */
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useCurrentDirectoryId } from '#/providers/DriveProvider'
import { useUser } from '$/providers/react'

/** Options for {@link useDirectoryIds}. */
export interface UseDirectoryIdsOptions {
  readonly category: Category
}

/** A hook returning the root directory id and expanded directory ids. */
export function useDirectoryIds(options: UseDirectoryIdsOptions) {
  const { category } = options

  const user = useUser()

  const rootDirectoryId = category.homeDirectoryId ?? user.rootDirectoryId
  /** The id of the directory to use in the "list directory" query. */
  const queryDirectoryId = useCurrentDirectoryId() ?? category.homeDirectoryId
  const currentDirectoryId = queryDirectoryId ?? rootDirectoryId

  return {
    rootDirectoryId,
    queryDirectoryId,
    currentDirectoryId,
  } as const
}
