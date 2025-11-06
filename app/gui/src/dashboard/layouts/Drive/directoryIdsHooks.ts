/** @file A hook returning the root directory id and expanded directory ids. */
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import { useCurrentDirectoryId } from '#/providers/DriveProvider'
import { useUser } from '$/providers/react'
import type { DirectoryId } from 'enso-common/src/services/Backend/types'

/** A hook returning the root directory id and expanded directory ids. */
export function useDirectoryIds(): {
  readonly rootDirectoryId: DirectoryId
  readonly queryDirectoryId: DirectoryId | null
  readonly currentDirectoryId: DirectoryId
} {
  const user = useUser()
  const { category } = useCategoriesAPI()
  const queryDirectoryIdRaw = useCurrentDirectoryId()

  const rootDirectoryId = category.homeDirectoryId ?? user.rootDirectoryId
  /** The id of the directory to use in the "list directory" query. */
  const queryDirectoryId = queryDirectoryIdRaw ?? category.homeDirectoryId
  const currentDirectoryId = queryDirectoryId ?? rootDirectoryId

  return {
    rootDirectoryId,
    queryDirectoryId,
    currentDirectoryId,
  }
}
