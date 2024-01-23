/** @file The categories available in the category switcher. */

// ================
// === Category ===
// ================

/** The categories available in the category switcher. */
enum Category {
  recent = 'Recent',
  drafts = 'Drafts',
  home = 'Home',
  root = 'Root',
  trash = 'Trash',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default Category
