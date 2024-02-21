/** @file The categories available in the category switcher. */

// ================
// === Category ===
// ================

/** The categories available in the category switcher. */
enum Category {
  recent = 'Recent',
  home = 'Home',
  trash = 'Trash',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default Category
