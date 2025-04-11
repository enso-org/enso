/** @file The categories available in the category switcher. */
import * as z from 'zod'

import type { UserId } from '#/services/Backend'
import {
  FilterBy,
  type DirectoryId,
  type Path,
  type User,
  type UserGroup,
  type UserGroupId,
} from '#/services/Backend'
import type { DropOperation } from '@react-types/shared'

const PATH_SCHEMA = z.string().refine((s): s is Path => true)
const DIRECTORY_ID_SCHEMA = z.string().refine((s): s is DirectoryId => true)

const EACH_CATEGORY_SCHEMA = z.object({
  label: z.string(),
  icon: z.string(),
})

/** A category corresponding to the root of the user or organization. */
const CLOUD_CATEGORY_SCHEMA = z
  .object({
    type: z.literal('cloud'),
    id: z.literal('cloud'),
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .merge(EACH_CATEGORY_SCHEMA)
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
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'local-directory': FilterBy.active,
}

/**
 * The type of the cached value for a category.
 * We use const enums because they compile to numeric values and they are faster than strings.
 */
const enum CategoryCacheType {
  cloud = 0,
  local = 1,
}

const CATEGORY_CACHE = new Map<Category['type'], CategoryCacheType>()

/** Whether the category is only accessible from the cloud. */
export function isCloudCategory(category: Category): category is AnyCloudCategory {
  const cached = CATEGORY_CACHE.get(category.type)

  if (cached != null) {
    return cached === CategoryCacheType.cloud
  }

  const result = ANY_CLOUD_CATEGORY_SCHEMA.safeParse(category)
  CATEGORY_CACHE.set(
    category.type,
    result.success ? CategoryCacheType.cloud : CategoryCacheType.local,
  )

  return result.success
}

/** Whether the category is only accessible locally. */
export function isLocalCategory(category: Category): category is AnyLocalCategory {
  const cached = CATEGORY_CACHE.get(category.type)

  if (cached != null) {
    return cached === CategoryCacheType.local
  }

  const result = ANY_LOCAL_CATEGORY_SCHEMA.safeParse(category)
  CATEGORY_CACHE.set(
    category.type,
    result.success ? CategoryCacheType.local : CategoryCacheType.cloud,
  )
  return result.success
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
): DropOperation {
  // Moving into the same category without a parentId is not allowed.
  if (from.type === to.type && parentId == null) {
    return 'cancel'
  }

  if (to.type === 'recent') {
    return 'cancel'
  }

  if (isCloudCategory(from) || isCloudCategory(to)) {
    if (isLocalCategory(from) || isLocalCategory(to)) {
      return 'cancel'
    }
  }

  switch (from.type) {
    case 'cloud':
    case 'recent':
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
      return 'move'
    }
  }
}
