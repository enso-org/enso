/** @file The categories available in the category switcher. */
import type * as backend from '#/services/Backend'
import * as z from 'zod'

// ====================
// === CategoryType ===
// ====================

/** The categories available in the category switcher. */
export enum CategoryType {
  cloud = 'cloud',
  local = 'local',
  recent = 'recent',
  trash = 'trash',
  user = 'user',
  team = 'team',
  localDirectory = 'local-directory',
}

// ================
// === Category ===
// ================

const PATH_SCHEMA = z.string().refine((s): s is backend.Path => true)
const DIRECTORY_ID_SCHEMA = z.string().refine((s): s is backend.DirectoryId => true)

/** A category with a special predefined meaning. */
export const STATIC_CATEGORY_SCHEMA = z
  .object({
    type: z.enum([CategoryType.cloud, CategoryType.local, CategoryType.recent, CategoryType.trash]),
  })
  .readonly()

/** A category with a special predefined meaning. */
interface StaticCategory extends z.infer<typeof STATIC_CATEGORY_SCHEMA> {}

/** A category corresponding to the root directory of a user. */
export const USER_CATEGORY_SCHEMA = z
  .object({
    type: z.literal(CategoryType.user),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .readonly()

/** A category corresponding to the root directory of a user. */
interface UserCategory extends z.infer<typeof USER_CATEGORY_SCHEMA> {}

export const TEAM_CATEGORY_SCHEMA = z
  .object({
    type: z.literal(CategoryType.team),
    team: z.custom<backend.UserGroupInfo>(() => true),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .readonly()

/** A category corresponding to the root directory of a team within an organization. */
interface TeamCategory extends z.infer<typeof TEAM_CATEGORY_SCHEMA> {}

export const LOCAL_DIRECTORY_CATEGORY_SCHEMA = z
  .object({
    type: z.literal(CategoryType.localDirectory),
    rootPath: PATH_SCHEMA,
    homeDirectoryId: DIRECTORY_ID_SCHEMA,
  })
  .readonly()

/** A category corresponding to an alternate local root directory. */
interface LocalDirectoryCategory extends z.infer<typeof LOCAL_DIRECTORY_CATEGORY_SCHEMA> {
  readonly type: CategoryType.localDirectory
  readonly rootPath: backend.Path
  readonly homeDirectoryId: backend.DirectoryId
}

export const CATEGORY_SCHEMA = z.union([
  LOCAL_DIRECTORY_CATEGORY_SCHEMA,
  STATIC_CATEGORY_SCHEMA,
  TEAM_CATEGORY_SCHEMA,
  USER_CATEGORY_SCHEMA,
])

/** A category of an arbitrary type. */
type Category = LocalDirectoryCategory | StaticCategory | TeamCategory | UserCategory
export default Category

// =======================
// === isCloudCategory ===
// =======================

/** Whether the category is only accessible from the cloud. */
export function isCloudCategory(category: Category) {
  return category.type !== CategoryType.local && category.type !== CategoryType.localDirectory
}

// =======================
// === isLocalCategory ===
// =======================

/** Whether the category is only accessible locally. */
export function isLocalCategory(category: Category) {
  return !isCloudCategory(category)
}

// ==========================
// === areCategoriesEqual ===
// ==========================

/** Whether the given categories are equal. */
export function areCategoriesEqual(a: Category, b: Category) {
  if (a.type !== b.type) {
    return false
  } else if (
    (a.type === CategoryType.user && b.type === CategoryType.user) ||
    (a.type === CategoryType.team && b.type === CategoryType.team)
  ) {
    return a.homeDirectoryId === b.homeDirectoryId
  } else {
    return true
  }
}
