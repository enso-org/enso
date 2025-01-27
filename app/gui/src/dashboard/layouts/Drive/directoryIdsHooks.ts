/** @file A hook returning the root directory id and expanded directory ids. */
import { useSuspenseQuery } from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import { Path, createRootDirectoryAsset } from 'enso-common/src/services/Backend'

import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useBackend } from '#/providers/BackendProvider'
import {
  useExpandedDirectoryIds,
  useReplaceExpandedDirectoryIds,
  useSetExpandedDirectoryIds,
} from '#/providers/DriveProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useCallback } from 'react'

/** Options for {@link useDirectoryIds}. */
export interface UseDirectoryIdsOptions {
  readonly category: Category
}

/** A hook returning the root directory id and expanded directory ids. */
export function useDirectoryIds(options: UseDirectoryIdsOptions) {
  const { category } = options
  const backend = useBackend(category)
  const { user } = useFullUserSession()

  const organizationQuery = useSuspenseQuery({
    queryKey: [backend.type, 'getOrganization'],
    queryFn: () => backend.getOrganization(),
  })

  const organization = organizationQuery.data

  /**
   * The expanded directories in the asset tree.
   * The root directory is not included as it might change when a user switches
   * between items in sidebar and we don't want to reset the expanded state using `useEffect`.
   */
  const privateExpandedDirectoryIds = useExpandedDirectoryIds()
  const setExpandedDirectoryIds = useSetExpandedDirectoryIds()
  const replaceExpandedDirectoryIds = useReplaceExpandedDirectoryIds()

  const [localRootDirectory] = useLocalStorageState('localRootDirectory')

  const rootDirectoryId = (() => {
    const localRootPath = localRootDirectory != null ? Path(localRootDirectory) : null
    const id =
      'homeDirectoryId' in category ?
        category.homeDirectoryId
      : backend.rootDirectoryId(user, organization, localRootPath)
    invariant(id, 'Missing root directory')
    return id
  })()

  const rootDirectory = createRootDirectoryAsset(rootDirectoryId)

  const expandedDirectoryIds = [rootDirectoryId].concat(
    privateExpandedDirectoryIds.filter((id) => id !== rootDirectoryId),
  )

  return {
    setExpandedDirectoryIds,
    rootDirectoryId,
    rootDirectory,
    expandedDirectoryIds,
    replaceExpandedDirectoryIds,
  } as const
}
