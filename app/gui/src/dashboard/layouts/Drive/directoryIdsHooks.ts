/** @file A hook returning the root directory id and expanded directory ids. */
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useCurrentDirectoryId, useSetCurrentDirectoryId } from '#/providers/DriveProvider'

/** Options for {@link useDirectoryIds}. */
export interface UseDirectoryIdsOptions {
  readonly category: Category
}

/** A hook returning the root directory id and expanded directory ids. */
export function useDirectoryIds(options: UseDirectoryIdsOptions) {
  const { category } = options
  const rootDirectoryId = category.homeDirectoryId
  const currentDirectoryId = useCurrentDirectoryId().current ?? rootDirectoryId
  const parentDirectoryId = useCurrentDirectoryId().parent ?? rootDirectoryId
  const setCurrentDirectoryId = useSetCurrentDirectoryId()

  return {
    setCurrentDirectoryId,
    rootDirectoryId,
    currentDirectoryId,
    parentDirectoryId,
  } as const
}
