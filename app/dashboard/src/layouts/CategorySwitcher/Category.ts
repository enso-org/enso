/** @file The categories available in the category switcher. */
import * as z from 'zod'

import AssetEventType from '#/events/AssetEventType'
import { useBackendQuery } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useDispatchAssetEvent } from '#/layouts/AssetsTable/EventListProvider'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend, useRemoteBackendStrict } from '#/providers/BackendProvider'
import type { AssetId, DirectoryId, Path, UserGroupInfo } from '#/services/Backend'
import { newDirectoryId } from '#/services/LocalBackend'
import invariant from 'tiny-invariant'

const PATH_SCHEMA = z.string().refine((s): s is Path => true)
const DIRECTORY_ID_SCHEMA = z.string().refine((s): s is DirectoryId => true)

/** A category corresponding to the root of the user or organization. */
const CLOUD_CATEGORY_SCHEMA = z.object({ type: z.literal('cloud') }).readonly()
/** A category corresponding to the root of the user or organization. */
export type CloudCategory = z.infer<typeof CLOUD_CATEGORY_SCHEMA>

/** A category containing recently opened Cloud projects. */
const RECENT_CATEGORY_SCHEMA = z.object({ type: z.literal('recent') }).readonly()
/** A category containing recently opened Cloud projects. */
export type RecentCategory = z.infer<typeof RECENT_CATEGORY_SCHEMA>

/** A category containing recently deleted Cloud items. */
const TRASH_CATEGORY_SCHEMA = z.object({ type: z.literal('trash') }).readonly()
/** A category containing recently deleted Cloud items. */
export type TrashCategory = z.infer<typeof TRASH_CATEGORY_SCHEMA>

/** A category corresponding to the root directory of a user. */
export const USER_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('user'),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .readonly()
/** A category corresponding to the root directory of a user. */
export type UserCategory = z.infer<typeof USER_CATEGORY_SCHEMA>

export const TEAM_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('team'),
    team: z.custom<UserGroupInfo>(() => true),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .readonly()
/** A category corresponding to the root directory of a team within an organization. */
export type TeamCategory = z.infer<typeof TEAM_CATEGORY_SCHEMA>

/** A category corresponding to the primary root directory for Local projects. */
const LOCAL_CATEGORY_SCHEMA = z.object({ type: z.literal('local') }).readonly()
/** A category corresponding to the primary root directory for Local projects. */
export type LocalCategory = z.infer<typeof LOCAL_CATEGORY_SCHEMA>

/** A category corresponding to an alternate local root directory. */
export const LOCAL_DIRECTORY_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('local-directory'),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .readonly()
/** A category corresponding to an alternate local root directory. */
export type LocalDirectoryCategory = z.infer<typeof LOCAL_DIRECTORY_CATEGORY_SCHEMA>

/** Any cloud category. */
export const ANY_CLOUD_CATEGORY_SCHEMA = z.union([
  CLOUD_CATEGORY_SCHEMA,
  RECENT_CATEGORY_SCHEMA,
  TRASH_CATEGORY_SCHEMA,
  TEAM_CATEGORY_SCHEMA,
  USER_CATEGORY_SCHEMA,
])
/** Any cloud category. */
export type AnyCloudCategory = z.infer<typeof ANY_CLOUD_CATEGORY_SCHEMA>

/** Any local category. */
export const ANY_LOCAL_CATEGORY_SCHEMA = z.union([
  LOCAL_CATEGORY_SCHEMA,
  LOCAL_DIRECTORY_CATEGORY_SCHEMA,
])
/** Any local category. */
export type AnyLocalCategory = z.infer<typeof ANY_LOCAL_CATEGORY_SCHEMA>

/** A category of an arbitrary type. */
export const CATEGORY_SCHEMA = z.union([ANY_CLOUD_CATEGORY_SCHEMA, ANY_LOCAL_CATEGORY_SCHEMA])
/** A category of an arbitrary type. */
export type Category = z.infer<typeof CATEGORY_SCHEMA>

/** Whether the category is only accessible from the cloud. */
export function isCloudCategory(category: Category): category is AnyCloudCategory {
  return ANY_CLOUD_CATEGORY_SCHEMA.safeParse(category).success
}

/** Whether the category is only accessible locally. */
export function isLocalCategory(category: Category): category is AnyLocalCategory {
  return ANY_LOCAL_CATEGORY_SCHEMA.safeParse(category).success
}

/** Whether the given categories are equal. */
export function areCategoriesEqual(a: Category, b: Category) {
  if (a.type !== b.type) {
    return false
  } else if (
    (a.type === 'user' && b.type === 'user') ||
    (a.type === 'team' && b.type === 'team') ||
    (a.type === 'local-directory' && b.type === 'local-directory')
  ) {
    return a.homeDirectoryId === b.homeDirectoryId
  } else {
    return true
  }
}

/** Whether an asset can be transferred between categories. */
export function canTransferBetweenCategories(from: Category, to: Category) {
  switch (from.type) {
    case 'cloud':
    case 'recent':
    case 'team':
    case 'user': {
      return to.type === 'trash' || to.type === 'cloud' || to.type === 'team' || to.type === 'user'
    }
    case 'trash': {
      return to.type === 'cloud'
    }
    case 'local':
    case 'local-directory': {
      return to.type === 'local' || to.type === 'local-directory'
    }
  }
}

/** A function to transfer a list of assets between categories. */
export function useTransferBetweenCategories() {
  const remoteBackend = useRemoteBackendStrict()
  const localBackend = useLocalBackend()
  const { user } = useFullUserSession()
  const { data: organization = null } = useBackendQuery(remoteBackend, 'getOrganization', [])
  const dispatchAssetEvent = useDispatchAssetEvent()
  return useEventCallback(
    (
      from: Category,
      to: Category,
      keys: Iterable<AssetId>,
      newParentKey?: DirectoryId | null,
      newParentId?: DirectoryId | null,
    ) => {
      switch (from.type) {
        case 'cloud':
        case 'recent':
        case 'team':
        case 'user': {
          if (to.type === 'trash') {
            dispatchAssetEvent({ type: AssetEventType.delete, ids: new Set(keys) })
          } else if (to.type === 'cloud' || to.type === 'team' || to.type === 'user') {
            newParentId ??=
              to.type === 'cloud' ?
                remoteBackend.rootDirectoryId(user, organization)
              : to.homeDirectoryId
            invariant(newParentId != null, 'The Cloud backend is missing a root directory.')
            newParentKey ??= newParentId
            dispatchAssetEvent({
              type: AssetEventType.move,
              newParentKey,
              newParentId,
              ids: new Set(keys),
            })
          }
          break
        }
        case 'trash': {
          dispatchAssetEvent({ type: AssetEventType.restore, ids: new Set(keys) })
          break
        }
        case 'local':
        case 'local-directory': {
          if (to.type === 'local' || to.type === 'local-directory') {
            const parentDirectory = to.type === 'local' ? localBackend?.rootPath() : to.rootPath
            invariant(parentDirectory != null, 'The Local backend is missing a root directory.')
            newParentId ??= newDirectoryId(parentDirectory)
            newParentKey ??= newParentId
            dispatchAssetEvent({
              type: AssetEventType.move,
              newParentKey,
              newParentId,
              ids: new Set(keys),
            })
          }
        }
      }
    },
  )
}
