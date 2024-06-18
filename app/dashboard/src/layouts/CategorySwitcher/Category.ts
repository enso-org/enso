/** @file The categories available in the category switcher. */

// ================
// === Category ===
// ================

/** The categories available in the category switcher. */
enum Category {
  cloud = 'cloud',
  local = 'local',
  recent = 'recent',
  trash = 'trash',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default Category

// ===============
// === isCloud ===
// ===============

/** Return `true` if the category is only accessible from the cloud.
 * Return `false` if the category is only accessibly locally. */
export function isCloud(category: Category) {
  return category !== Category.local
}
