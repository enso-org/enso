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
