/** @file The categories available in the category switcher. */
import type * as backend from '#/services/Backend'

// ====================
// === CategoryType ===
// ====================

/** A category with a special predefined meaning. */
interface StaticCategory {
  readonly type: Exclude<CategoryType, CategoryType.team | CategoryType.user>
}

/** A category corresponding to the root directory of a user. */
interface UserCategory {
  readonly type: CategoryType.user
  readonly rootPath: backend.Path
  readonly homeDirectoryId: backend.DirectoryId
}

/** A category corresponding to the root directory of a team within an organization. */
interface TeamCategory {
  readonly type: CategoryType.team
  readonly team: backend.UserGroupInfo
  readonly rootPath: backend.Path
  readonly homeDirectoryId: backend.DirectoryId
}

/** A category of an arbitrary type. */
type Category = StaticCategory | TeamCategory | UserCategory
export default Category

// ================
// === Category ===
// ================

/** The categories available in the category switcher. */
export enum CategoryType {
  cloud = 'cloud',
  local = 'local',
  recent = 'recent',
  trash = 'trash',
  user = 'user',
  team = 'team',
}

// =======================
// === isCloudCategory ===
// =======================

/** Whether the category is only accessible from the cloud. */
export function isCloudCategory(category: Category) {
  return category.type !== CategoryType.local
}

// =======================
// === isLocalCategory ===
// =======================

/** Whether the category is only accessible locally. */
export function isLocalCategory(category: Category) {
  return category.type === CategoryType.local
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
