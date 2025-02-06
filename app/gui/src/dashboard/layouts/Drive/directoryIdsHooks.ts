/** @file A hook returning the root directory id and expanded directory ids. */
import { useSuspenseQuery } from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import { Path, createRootDirectoryAsset } from 'enso-common/src/services/Backend'

import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useBackend } from '#/providers/BackendProvider'
import {
  useCurrentDirectoryId,
  useSetCurrentDirectoryId,
  useSetExpandedDirectoryIds,
} from '#/providers/DriveProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'

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

  const setExpandedDirectoryIds = useSetExpandedDirectoryIds()

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

  const currentDirectoryId = useCurrentDirectoryId().current ?? rootDirectoryId
  const parentDirectoryId = useCurrentDirectoryId().parent ?? rootDirectoryId
  const setCurrentDirectoryId = useSetCurrentDirectoryId()

  return {
    setExpandedDirectoryIds,
    setCurrentDirectoryId,
    rootDirectoryId,
    rootDirectory,
    currentDirectoryId,
    parentDirectoryId,
  } as const
}
