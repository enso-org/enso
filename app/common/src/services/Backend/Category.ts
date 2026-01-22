/** @file The categories available in the category switcher. */
import * as z from 'zod'
import {
  BackendType,
  FilterBy,
  type DirectoryId,
  type Path,
  type User,
  type UserGroup,
  type UserGroupId,
  type UserId,
} from '../Backend.js'

// oxlint-disable-next-line no-unused-vars
const PATH_SCHEMA = z.string().refine((s): s is Path => true)
// oxlint-disable-next-line no-unused-vars
const DIRECTORY_ID_SCHEMA = z.string().refine((s): s is DirectoryId => true)

const EACH_CATEGORY_SCHEMA = z.object({
  label: z.string(),
  icon: z.string(),
  canUploadHere: z.boolean(),
  /**
   * Internal type discriminator.
   * Used to determine the type of the category without having to check for any other properties.
   */
  backend: z.nativeEnum(BackendType),
})

/** A category corresponding to the root of the user or organization. */
const CLOUD_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('cloud'),
    id: z.literal('cloud'),
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .merge(EACH_CATEGORY_SCHEMA)
  .merge(z.object({ backend: z.literal(BackendType.remote) }))
  .readonly()
/** A category corresponding to the root of the user or organization. */
export type CloudCategory = z.infer<typeof CLOUD_CATEGORY_SCHEMA>

/** A category containing recently opened Cloud projects. */
const RECENT_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('recent'),
    id: z.literal('recent'),
    homeDirectoryId: z.null(),
  })
  .merge(EACH_CATEGORY_SCHEMA)
  .merge(z.object({ backend: z.literal(BackendType.remote) }))
  .readonly()
/** A category containing recently opened Cloud projects. */
export type RecentCategory = z.infer<typeof RECENT_CATEGORY_SCHEMA>

/** A category containing recently deleted Cloud items. */
const TRASH_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('trash'),
    id: z.literal('trash'),
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .merge(EACH_CATEGORY_SCHEMA)
  .merge(z.object({ backend: z.literal(BackendType.remote) }))
  .readonly()
/** A category containing recently deleted Cloud items. */
export type TrashCategory = z.infer<typeof TRASH_CATEGORY_SCHEMA>

/** A category corresponding to the root directory of a user. */
export const USER_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('user'),
    user: z.custom<User>(() => true),
    id: z.custom<UserId>(() => true),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .merge(EACH_CATEGORY_SCHEMA)
  .merge(z.object({ backend: z.literal(BackendType.remote) }))
  .readonly()
/** A category corresponding to the root directory of a user. */
export type UserCategory = z.infer<typeof USER_CATEGORY_SCHEMA>

export const TEAM_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('team'),
    id: z.custom<UserGroupId>(() => true),
    team: z.custom<UserGroup>(() => true),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .merge(EACH_CATEGORY_SCHEMA)
  .merge(z.object({ backend: z.literal(BackendType.remote) }))
  .readonly()
/** A category corresponding to the root directory of a team within an organization. */
export type TeamCategory = z.infer<typeof TEAM_CATEGORY_SCHEMA>

/** A category corresponding to the primary root directory for Local projects. */

const LOCAL_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('local'),
    id: z.literal('local'),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .merge(EACH_CATEGORY_SCHEMA)
  .merge(z.object({ backend: z.literal(BackendType.local) }))
  .readonly()
/** A category corresponding to the primary root directory for Local projects. */
export type LocalCategory = z.infer<typeof LOCAL_CATEGORY_SCHEMA>

/** A category corresponding to an alternate local root directory. */
export const LOCAL_DIRECTORY_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('local-directory'),
    id: z.custom<DirectoryId>(() => true),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .merge(EACH_CATEGORY_SCHEMA)
  .merge(z.object({ backend: z.literal(BackendType.local) }))
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

/** Any category. */
export type AnyCategory = AnyCloudCategory | AnyLocalCategory

/** A category of an arbitrary type. */
export const CATEGORY_SCHEMA = z.union([ANY_CLOUD_CATEGORY_SCHEMA, ANY_LOCAL_CATEGORY_SCHEMA])
/** A category of an arbitrary type. */
export type Category = z.infer<typeof CATEGORY_SCHEMA>

/** The `id` of a {@link Category}. */
export type CategoryId = Category['id']

/** An inferred Category type from a specific type. */
export type CategoryByType<T extends Category['type']> = Extract<Category, { type: T }>

export const CATEGORY_TO_FILTER_BY: Readonly<Record<Category['type'], FilterBy | null>> = {
  cloud: FilterBy.active,
  local: FilterBy.active,
  recent: null,
  trash: FilterBy.trashed,
  user: FilterBy.active,
  team: FilterBy.active,
  'local-directory': FilterBy.active,
}

/** Whether the category is only accessible from the cloud. */
export function isCloudCategory(category: Category): category is AnyCloudCategory {
  return category.backend === BackendType.remote
}

/** Whether the category is only accessible locally. */
export function isLocalCategory(category: Category): category is AnyLocalCategory {
  return category.backend === BackendType.local
}

/** Whether the given categories are equal. */
export function areCategoriesEqual(a: Category, b: Category) {
  return a.id === b.id
}

/** Whether an asset can be transferred between categories. */
export function canTransferBetweenCategories(
  from: Category,
  to: Category,
  parentId: DirectoryId | null = null,
) {
  const operation = dropOperationBetweenCategories(from, to, parentId)

  return operation !== 'cancel'
}

/**
 * The drop operation to use when transferring assets between categories.
 * @param from - The category to transfer from.
 * @param to - The category to transfer to.
 * @returns The drop operation to use.
 */
export function dropOperationBetweenCategories(
  from: Category,
  to: Category,
  parentId: DirectoryId | null = null,
): 'cancel' | 'copy' | 'move' | undefined {
  // Moving into the same category without a parentId is not allowed.
  if (from.type === to.type && parentId == null) {
    return 'cancel'
  }

  if (to.type === 'recent' || from.type === 'recent') {
    return 'cancel'
  }

  if (isLocalCategory(from)) {
    if (to.type === 'trash') {
      return 'cancel'
    }
  }

  if (isCloudCategory(from) || isCloudCategory(to)) {
    if (isLocalCategory(from) || isLocalCategory(to)) {
      return 'copy'
    }
  }

  switch (from.type) {
    case 'cloud':
    case 'user': {
      return 'move'
    }
    case 'team': {
      if (to.type === 'trash') {
        return 'move'
      }

      return 'copy'
    }
    case 'trash': {
      return 'move'
    }
    case 'local':
    case 'local-directory': {
      if (isCloudCategory(to)) {
        return 'copy'
      }

      return 'move'
    }
  }
}
