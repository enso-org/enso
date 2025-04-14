/** @file A hook returning the root directory id and expanded directory ids. */
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useUser } from '#/providers/AuthProvider'
import { useCurrentDirectoryId, useSetCurrentDirectoryId } from '#/providers/DriveProvider'

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
  const queryDirectoryId = useCurrentDirectoryId().current ?? category.homeDirectoryId
  const currentDirectoryId = queryDirectoryId ?? rootDirectoryId
  const parentDirectoryId = useCurrentDirectoryId().parent ?? rootDirectoryId
  const setCurrentDirectoryId = useSetCurrentDirectoryId()

  return {
    setCurrentDirectoryId,
    rootDirectoryId,
    queryDirectoryId,
    currentDirectoryId,
    parentDirectoryId,
  } as const
}
